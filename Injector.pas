unit Injector;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, TlHelp32;

type
  TNTSignature = DWORD;
  PNTSignature = ^TNTSignature;

  TImageBaseRelocation = record
    VirtualAddress: DWORD;
    SizeOfBlock: DWORD;
  end;
  PImageBaseRelocation = ^TImageBaseRelocation;

  {$IFDEF WIN64}
  TImageOptionalHeader = TImageOptionalHeader64;
  {$ELSE}
  TImageOptionalHeader = TImageOptionalHeader32;
  {$ENDIF}
  PImageOptionalHeader = ^TImageOptionalHeader;

  {$IFDEF WIN64}
  TImageThunkData = TImageThunkData64;
  {$ELSE}
  TImageThunkData = TImageThunkData32;
  {$ENDIF}
  PImageThunkData = ^TImageThunkData;

  PRelocationInfo =
  {$IFDEF WIN64}
    PCardinal
  {$ELSE}
    PWord
  {$ENDIF};

  TPEHeader = record
    pImageBase            : Pointer;
    _pImageDosHeader      : PImageDosHeader;
    _pNTSignature         : PNTSignature;
    _pImageFileHeader     : PImageFileHeader;
    _pImageOptionalHeader : PImageOptionalHeader;
    _pImageSectionHeader  : PImageSectionHeader;
    SectionHeaderCount    : Cardinal;
    pSectionHeaders       : array of PImageSectionHeader;
  end;

  TPEHeaderDirectories = record
    _pImageExportDirectory : PImageExportDirectory;
  end;

  EInjectorException = class(Exception)
  private
    FLastError: Integer;
  public
    constructor Create(const WinAPI: String); overload;
    property LastError: Integer read FLastError;
  end;

  TInjector = class
  private
    function GetProcessIdByName(const AProcessName: String): Cardinal;
    function RVAToVA(const pImageBase: Pointer; const ARelativeVirtualAddress: NativeUInt): Pointer;
    function IdentifyPEHeader(const pImageBase: Pointer): TPEHeader;
    function IdentifyPEHeaderDirectories(const APEHeader: TPEHeader): TPEHeaderDirectories;
    procedure ResolveImportTable(const APEHeader: TPEHeader);
    procedure PerformBaseRelocation(const APEHeader: TPEHeader; const ADelta: NativeUInt);

    // Remote helper methods
    function WriteMemory(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: SIZE_T): Boolean;
    function PerformRemoteBaseRelocation(hProcess: THandle; const APEHeader: TPEHeader; const RemoteBase: Pointer; const ADelta: NativeUInt): Boolean;
    function ResolveRemoteImportTable(hProcess: THandle; const APEHeader: TPEHeader; const RemoteBase: Pointer): Boolean;

    // Shellcode Loader injection methods
    function InjectViaShellLoader(hProcess: THandle; const ADLLFile: String): Boolean;

  public

    function LoadLibrary(const ADLLFile: String; const ATargetProcess: String): Boolean; overload;
    function LoadLibrary(const ADLLFile: String; const ATargetProcessId: Cardinal): Boolean; overload;

    function ReflectFromMemory(const pSourceBuffer: Pointer; const ABufferSize: UInt): Pointer;
    function ReflectFromFile(const AFileName: String): Pointer;
    function GetReflectedProcAddress(const pImageBase: Pointer; const AFunctionOrOrdinal: String): Pointer;
    function ReflectFromMemStream(const AStream: TMemoryStream): Pointer;

    function ReflectInjectRemote(const pReflectedBase: Pointer; const ATargetProcess: String): Boolean;

    // PoolParty injection
    procedure InjectShellLoader(const ADLLFile: String; const ATargetProcess: String);
  end;

const
  // Named constant for OptionalHeader image base offset (after DOS, NT Signature, FileHeader and Standard Fields)
  OPTIONAL_HEADER_IMAGE_BASE_OFFSET = SizeOf(TNTSignature) + SizeOf(TImageFileHeader) + 24;
  IMAGE_REL_BASED_DIR64 = 10;

implementation

{ EInjectorException }

constructor EInjectorException.Create(const WinAPI: String);
var
  AFormattedMessage: String;
begin
  FLastError := GetLastError();
  AFormattedMessage := Format('%s: last_err=%d, last_err_msg="%s".',
    [WinAPI, FLastError, SysErrorMessage(FLastError)]);
  inherited Create(AFormattedMessage);
end;

{ TInjector Helper Methods }

function TInjector.GetProcessIdByName(const AProcessName: String): Cardinal;
var
  Snapshot: THandle;
  ProcessEntry: TProcessEntry32;
begin
  Result := 0;
  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if Snapshot = INVALID_HANDLE_VALUE then
    raise EInjectorException.Create('CreateToolhelp32Snapshot');
  try
    ProcessEntry.dwSize := SizeOf(TProcessEntry32);
    if Process32First(Snapshot, ProcessEntry) then
    begin
      repeat
        if SameText(ProcessEntry.szExeFile, AProcessName) then
        begin
          Result := ProcessEntry.th32ProcessID;
          Exit;
        end;
      until not Process32Next(Snapshot, ProcessEntry);
    end;
  finally
    CloseHandle(Snapshot);
  end;

  if Result = 0 then
    raise Exception.CreateFmt('Process "%s" not found.', [AProcessName]);
end;

function TInjector.WriteMemory(hProcess: THandle; lpBaseAddress, lpBuffer: Pointer; nSize: SIZE_T): Boolean;
var
  BytesWritten: SIZE_T;
begin
  Result := WriteProcessMemory(hProcess, lpBaseAddress, lpBuffer, nSize, BytesWritten);
  if not Result then
    raise EInjectorException.Create('WriteProcessMemory');
end;

function TInjector.RVAToVA(const pImageBase: Pointer; const ARelativeVirtualAddress: NativeUInt): Pointer;
begin
  Result := Pointer(NativeUInt(pImageBase) + ARelativeVirtualAddress);
end;

function TInjector.IdentifyPEHeader(const pImageBase: Pointer): TPEHeader;
var
  pOffset: Pointer;
  _pImageSectionHeader: PImageSectionHeader;
  I: Cardinal;
  procedure IncOffset(const AIncrement: Cardinal);
  begin
    pOffset := Pointer(NativeUInt(pOffset) + AIncrement);
  end;
begin
  ZeroMemory(@Result, SizeOf(TPEHeader));
  if not Assigned(pImageBase) then
    Exit;
  Result.pImageBase := pImageBase;
  pOffset := Result.pImageBase;

  Result._pImageDosHeader := PImageDosHeader(pOffset);
  if Result._pImageDosHeader.e_magic <> IMAGE_DOS_SIGNATURE then
    Exit;

  IncOffset(Result._pImageDosHeader._lfanew);
  Result._pNTSignature := PNTSignature(pOffset);
  if Result._pNTSignature^ <> IMAGE_NT_SIGNATURE then
    Exit;

  IncOffset(SizeOf(TNTSignature));
  Result._pImageFileHeader := PImageFileHeader(pOffset);
  IncOffset(SizeOf(TImageFileHeader));
  Result._pImageOptionalHeader := PImageOptionalHeader(pOffset);

  Result.SectionHeaderCount := Result._pImageFileHeader^.NumberOfSections;
  SetLength(Result.pSectionHeaders, Result.SectionHeaderCount);
  IncOffset(SizeOf(TImageOptionalHeader));
  for I := 0 to Result.SectionHeaderCount - 1 do
  begin
    _pImageSectionHeader := PImageSectionHeader(pOffset);
    Result.pSectionHeaders[I] := _pImageSectionHeader;
    IncOffset(SizeOf(TImageSectionHeader));
  end;
end;

function TInjector.IdentifyPEHeaderDirectories(const APEHeader: TPEHeader): TPEHeaderDirectories;
var
  AVirtualAddress: Cardinal;
begin
  ZeroMemory(@Result, SizeOf(TPEHeaderDirectories));
  AVirtualAddress := APEHeader._pImageOptionalHeader^.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress;
  if AVirtualAddress <> 0 then
    Result._pImageExportDirectory := RVAToVA(APEHeader.pImageBase, AVirtualAddress);
end;

procedure TInjector.ResolveImportTable(const APEHeader: TPEHeader);
var
  _pImageDataDirectory: PImageDataDirectory;
  _pImageImportDescriptor: PImageImportDescriptor;
  hModule: THandle;
  _pImageOriginalThunkData, _pImageFirstThunkData: PImageThunkData;
  pFunction, pProcName: Pointer;
  function RVA(const Offset: NativeUInt): Pointer;
  begin
    Result := RVAToVA(APEHeader.pImageBase, Offset);
  end;
begin
  _pImageDataDirectory := @APEHeader._pImageOptionalHeader^.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  if _pImageDataDirectory^.Size = 0 then
    Exit;
  _pImageImportDescriptor := RVA(_pImageDataDirectory^.VirtualAddress);
  while _pImageImportDescriptor^.Name <> 0 do
  begin
    hModule := LoadLibraryA(RVA(_pImageImportDescriptor^.Name));
    if hModule = 0 then
    begin
      Inc(_pImageImportDescriptor);
      Continue;
    end;
    if _pImageImportDescriptor^.OriginalFirstThunk <> 0 then
      _pImageOriginalThunkData := RVA(_pImageImportDescriptor^.OriginalFirstThunk)
    else
      _pImageOriginalThunkData := RVA(_pImageImportDescriptor^.FirstThunk);
    _pImageFirstThunkData := RVA(_pImageImportDescriptor^.FirstThunk);
    if not Assigned(_pImageOriginalThunkData) then
    begin
      FreeLibrary(hModule);
      Inc(_pImageImportDescriptor);
      Continue;
    end;
    while _pImageOriginalThunkData^.AddressOfData <> 0 do
    begin
      if (_pImageOriginalThunkData^.Ordinal and IMAGE_ORDINAL_FLAG) <> 0 then
        pProcName := MAKEINTRESOURCE(_pImageOriginalThunkData^.Ordinal and $FFFF)
      else
        pProcName := RVA(_pImageOriginalThunkData^.AddressOfData + SizeOf(Word));
      pFunction := GetProcAddress(hModule, PAnsiChar(pProcName));
      if Assigned(pFunction) then
        _pImageFirstThunkData^._Function := NativeUInt(pFunction);
      Inc(_pImageOriginalThunkData);
      Inc(_pImageFirstThunkData);
    end;
    FreeLibrary(hModule);
    Inc(_pImageImportDescriptor);
  end;
end;

procedure TInjector.PerformBaseRelocation(const APEHeader: TPEHeader; const ADelta: NativeUInt);
var
  I: Cardinal;
  _pImageDataDirectory: PImageDataDirectory;
  pRelocationTable: PImageBaseRelocation;
  pRelocationAddress: Pointer;
  pRelocInfo: PRelocationInfo;
  pRelocationType, pRelocationOffset: NativeUInt;
  ARelocationCount: Cardinal;
const
  IMAGE_SIZEOF_BASE_RELOCATION = 8;
  IMAGE_REL_BASED_HIGH         = 1;
  IMAGE_REL_BASED_LOW          = 2;
  IMAGE_REL_BASED_HIGHLOW      = 3;
begin
  _pImageDataDirectory := @APEHeader._pImageOptionalHeader^.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC];
  if _pImageDataDirectory^.Size = 0 then
    Exit;
  pRelocationTable := RVAToVA(APEHeader.pImageBase, _pImageDataDirectory^.VirtualAddress);
  while pRelocationTable^.VirtualAddress > 0 do
  begin
    pRelocationAddress := RVAToVA(APEHeader.pImageBase, pRelocationTable^.VirtualAddress);
    pRelocInfo := RVAToVA(pRelocationTable, IMAGE_SIZEOF_BASE_RELOCATION);
    ARelocationCount := (pRelocationTable^.SizeOfBlock - SizeOf(TImageBaseRelocation)) div SizeOf(Word);
    for I := 0 to ARelocationCount - 1 do
    begin
      pRelocationType := pRelocInfo^ shr 12;
      pRelocationOffset := pRelocInfo^ and $FFF;
      case pRelocationType of
        IMAGE_REL_BASED_HIGHLOW, IMAGE_REL_BASED_DIR64:
          Inc(PNativeUInt(NativeUInt(pRelocationAddress) + pRelocationOffset)^, ADelta);
        IMAGE_REL_BASED_HIGH:
          Inc(PNativeUInt(NativeUInt(pRelocationAddress) + pRelocationOffset)^, HiWord(ADelta));
        IMAGE_REL_BASED_LOW:
          Inc(PNativeUInt(NativeUInt(pRelocationAddress) + pRelocationOffset)^, LoWord(ADelta));
      end;
      Inc(pRelocInfo);
    end;
    pRelocationTable := Pointer(NativeUInt(pRelocationTable) + pRelocationTable^.SizeOfBlock);
  end;
end;

function TInjector.ReflectFromMemory(const pSourceBuffer: Pointer; const ABufferSize: UInt): Pointer;
var
  ASourcePEHeader, ADestPEHeader: TPEHeader;
  pImageBase: Pointer;
  I: Cardinal;
  _pImageSectionHeader: PImageSectionHeader;
  ADelta: UInt64;
begin
  Result := nil;
  ASourcePEHeader := IdentifyPEHeader(pSourceBuffer);
  {$IFDEF WIN64}
  if ASourcePEHeader._pImageFileHeader^.Machine <> IMAGE_FILE_MACHINE_AMD64 then
    raise Exception.Create('Architecture mismatch!');
  {$ELSE}
  if ASourcePEHeader._pImageFileHeader^.Machine <> IMAGE_FILE_MACHINE_I386 then
    raise Exception.Create('Architecture mismatch!');
  {$ENDIF}
  pImageBase := VirtualAlloc(nil, ASourcePEHeader._pImageOptionalHeader^.SizeOfImage, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if not Assigned(pImageBase) then
    Exit;
  CopyMemory(pImageBase, pSourceBuffer, ASourcePEHeader._pImageOptionalHeader^.SizeOfHeaders);
  for I := 0 to ASourcePEHeader.SectionHeaderCount - 1 do
  begin
    _pImageSectionHeader := ASourcePEHeader.pSectionHeaders[I];
    ZeroMemory(Pointer(NativeUInt(pImageBase) + _pImageSectionHeader^.VirtualAddress), _pImageSectionHeader^.Misc.VirtualSize);
    CopyMemory(Pointer(NativeUInt(pImageBase) + _pImageSectionHeader^.VirtualAddress),
      Pointer(NativeUInt(pSourceBuffer) + _pImageSectionHeader^.PointerToRawData),
      _pImageSectionHeader^.SizeOfRawData);
  end;
  ADelta := NativeUInt(pImageBase) - ASourcePEHeader._pImageOptionalHeader^.ImageBase;
  ADestPEHeader := IdentifyPEHeader(pImageBase);
  ADestPEHeader._pImageOptionalHeader^.ImageBase := NativeUInt(pImageBase);
  ResolveImportTable(ADestPEHeader);
  if ADelta <> 0 then
    PerformBaseRelocation(ADestPEHeader, ADelta);
  Result := pImageBase;
end;

function TInjector.ReflectFromFile(const AFileName: String): Pointer;
var
  AFileStream: TFileStream;
  pBuffer: Pointer;
begin
  Result := nil;
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    GetMem(pBuffer, AFileStream.Size);
    try
      AFileStream.ReadBuffer(pBuffer^, AFileStream.Size);
      Result := ReflectFromMemory(pBuffer, AFileStream.Size);
    finally
      FreeMem(pBuffer);
    end;
  finally
    AFileStream.Free;
  end;
end;

function TInjector.GetReflectedProcAddress(const pImageBase: Pointer; const AFunctionOrOrdinal: String): Pointer;
var
  APEHeader: TPEHeader;
  APEHeaderDirectories: TPEHeaderDirectories;
  I: Cardinal;
  pOffset: PCardinal;
  pAddrOfNameOrdinals, pAddrOfFunctions, pAddrOfNames: Pointer;
  ACurrentName: String;
  AOrdinalCandidate: Integer;
  ACurrentOrdinal: Word;
  AResolveByName: Boolean;
  pFuncAddress: PCardinal;
begin
  Result := nil;
  if not Assigned(pImageBase) then
    Exit;
  APEHeader := IdentifyPEHeader(pImageBase);
  APEHeaderDirectories := IdentifyPEHeaderDirectories(APEHeader);
  if not Assigned(APEHeaderDirectories._pImageExportDirectory) then
    Exit;
  pAddrOfNameOrdinals := Pointer(NativeUInt(APEHeader.pImageBase) + APEHeaderDirectories._pImageExportDirectory^.AddressOfNameOrdinals);
  pAddrOfFunctions := Pointer(NativeUInt(APEHeader.pImageBase) + APEHeaderDirectories._pImageExportDirectory^.AddressOfFunctions);
  pAddrOfNames := Pointer(NativeUInt(APEHeader.pImageBase) + APEHeaderDirectories._pImageExportDirectory^.AddressOfNames);
  AResolveByName := not TryStrToInt(AFunctionOrOrdinal, AOrdinalCandidate);
  if (AOrdinalCandidate < Low(Word)) or (AOrdinalCandidate > High(Word)) and not AResolveByName then
    AResolveByName := True;
  for I := 0 to APEHeaderDirectories._pImageExportDirectory^.NumberOfNames - 1 do
  begin
    pOffset := Pointer(NativeUInt(pAddrOfNames) + (I * SizeOf(Cardinal)));
    ACurrentName := String(PAnsiChar(NativeUInt(pImageBase) + pOffset^));
    ACurrentOrdinal := PWord(NativeUInt(pAddrOfNameOrdinals) + (I * SizeOf(Word)))^;
    if AResolveByName then
    begin
      if (String.Compare(ACurrentName, AFunctionOrOrdinal, True) <> 0) then
        Continue;
    end
    else
    begin
      if (ACurrentOrdinal + APEHeaderDirectories._pImageExportDirectory^.Base) <> AOrdinalCandidate then
        Continue;
    end;
    pFuncAddress := PCardinal(NativeUInt(pAddrOfFunctions) + (ACurrentOrdinal * SizeOf(Cardinal)));
    Result := Pointer(NativeUInt(pImageBase) + pFuncAddress^);
    Break;
  end;
end;

function TInjector.ReflectFromMemStream(const AStream: TMemoryStream): Pointer;
begin
  Result := nil;
  if not Assigned(AStream) or (AStream.Size = 0) then
    Exit;
  Result := ReflectFromMemory(AStream.Memory, AStream.Size);
end;

{ Remote operations }

function TInjector.PerformRemoteBaseRelocation(hProcess: THandle; const APEHeader: TPEHeader; const RemoteBase: Pointer; const ADelta: NativeUInt): Boolean;
var
  _pImageDataDirectory: PImageDataDirectory;
  pRelocationTable: PImageBaseRelocation;
  ARelocationCount: Cardinal;
  I: Cardinal;
  pRelocInfo: PRelocationInfo;
  pRelocationAddress: NativeUInt;
  pRelocationType, pRelocationOffset: NativeUInt;

  function ReadUIntPtr(Addr: Pointer; out Value: NativeUInt): Boolean;
  var
    BytesRead: SIZE_T;
  begin
    Result := ReadProcessMemory(hProcess, Addr, @Value, SizeOf(Value), BytesRead);
  end;

  function WriteUIntPtr(Addr: Pointer; Value: NativeUInt): Boolean;
  var
    BytesWritten: SIZE_T;
  begin
    Result := WriteProcessMemory(hProcess, Addr, @Value, SizeOf(Value), BytesWritten);
  end;

  function ReadWordValue(Addr: Pointer; out w: Word): Boolean;
  var
    BytesRead: SIZE_T;
  begin
    Result := ReadProcessMemory(hProcess, Addr, @w, SizeOf(Word), BytesRead);
  end;

  function ReadRelocTable(Addr: Pointer; out Table: TImageBaseRelocation): Boolean;
  var
    BytesRead: SIZE_T;
  begin
    Result := ReadProcessMemory(hProcess, Addr, @Table, SizeOf(Table), BytesRead);
  end;
const
  IMAGE_SIZEOF_BASE_RELOCATION = 8;
  IMAGE_REL_BASED_HIGH         = 1;
  IMAGE_REL_BASED_LOW          = 2;
  IMAGE_REL_BASED_HIGHLOW      = 3;
begin
  Result := True;
  _pImageDataDirectory := @APEHeader._pImageOptionalHeader^.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC];
  if _pImageDataDirectory^.Size = 0 then
    Exit(True);
  pRelocationTable := Pointer(NativeUInt(RemoteBase) + _pImageDataDirectory^.VirtualAddress);
  while True do
  begin
    var Table: TImageBaseRelocation;
    if not ReadRelocTable(pRelocationTable, Table) then Break;
    if Table.VirtualAddress = 0 then Break;
    pRelocInfo := Pointer(NativeUInt(pRelocationTable) + IMAGE_SIZEOF_BASE_RELOCATION);
    ARelocationCount := (Table.SizeOfBlock - SizeOf(TImageBaseRelocation)) div SizeOf(Word);
    for I := 0 to ARelocationCount - 1 do
    begin
      var RawWord: Word := 0;
      if not ReadWordValue(pRelocInfo, RawWord) then
      begin
        Result := False; Exit;
      end;
      pRelocationType := RawWord shr 12;
      pRelocationOffset := RawWord and $FFF;
      pRelocationAddress := NativeUInt(RemoteBase) + Table.VirtualAddress + pRelocationOffset;
      case pRelocationType of
        IMAGE_REL_BASED_HIGHLOW, IMAGE_REL_BASED_DIR64:
          begin
            var PtrVal: NativeUInt := 0;
            if not ReadUIntPtr(Pointer(pRelocationAddress), PtrVal) then
            begin
              Result := False; Exit;
            end;
            Inc(PtrVal, ADelta);
            if not WriteUIntPtr(Pointer(pRelocationAddress), PtrVal) then
            begin
              Result := False; Exit;
            end;
          end;
        IMAGE_REL_BASED_HIGH:
          begin
            var PtrVal: NativeUInt := 0;
            if not ReadUIntPtr(Pointer(pRelocationAddress), PtrVal) then
            begin
              Result := False; Exit;
            end;
            Inc(PtrVal, HiWord(ADelta));
            if not WriteUIntPtr(Pointer(pRelocationAddress), PtrVal) then
            begin
              Result := False; Exit;
            end;
          end;
        IMAGE_REL_BASED_LOW:
          begin
            var PtrVal: NativeUInt := 0;
            if not ReadUIntPtr(Pointer(pRelocationAddress), PtrVal) then
            begin
              Result := False; Exit;
            end;
            Inc(PtrVal, LoWord(ADelta));
            if not WriteUIntPtr(Pointer(pRelocationAddress), PtrVal) then
            begin
              Result := False; Exit;
            end;
          end;
      end;
      pRelocInfo := Pointer(NativeUInt(pRelocInfo) + SizeOf(Word));
    end;
    pRelocationTable := Pointer(NativeUInt(pRelocationTable) + Table.SizeOfBlock);
  end;
end;

function TInjector.ResolveRemoteImportTable(hProcess: THandle; const APEHeader: TPEHeader; const RemoteBase: Pointer): Boolean;
var
  _pImageDataDirectory: PImageDataDirectory;
  _pImageImportDescriptor: TImageImportDescriptor;
  ImportDescAddr: Pointer;

  function RVA(Offset: NativeUInt): NativeUInt;
  begin
    Result := NativeUInt(RemoteBase) + Offset;
  end;

  function ReadImportDescriptor(Addr: Pointer; out Desc: TImageImportDescriptor): Boolean;
  var
    BytesRead: SIZE_T;
  begin
    Result := ReadProcessMemory(hProcess, Addr, @Desc, SizeOf(Desc), BytesRead);
  end;

  function ReadPtr(Addr: Pointer): NativeUInt;
  var
    BytesRead: SIZE_T;
    Value: NativeUInt;
  begin
    Value := 0;
    if ReadProcessMemory(hProcess, Addr, @Value, SizeOf(Value), BytesRead) then
      Result := Value
    else
      Result := 0;
  end;

  function WritePtr(Addr: Pointer; Value: NativeUInt): Boolean;
  var
    BytesWritten: SIZE_T;
  begin
    Result := WriteProcessMemory(hProcess, Addr, @Value, SizeOf(Value), BytesWritten);
  end;

  function ReadStrA(Addr: Pointer): AnsiString;
  var
    ch: AnsiChar;
    i: Integer;
    BytesRead: SIZE_T;
  begin
    Result := '';
    i := 0;
    repeat
      if not ReadProcessMemory(hProcess, Pointer(NativeUInt(Addr)+i), @ch, 1, BytesRead) then
        Break;
      if ch = #0 then
        Break;
      Result := Result + ch;
      Inc(i);
    until False;
  end;

  function GetOrdinalFlag: NativeUInt;
  begin
    {$IFDEF WIN64}
    Result := IMAGE_ORDINAL_FLAG64;
    {$ELSE}
    Result := IMAGE_ORDINAL_FLAG32;
    {$ENDIF}
  end;
var
  DllName, ImportName: AnsiString;
  DllNamePtr: Pointer;
  hModule: THandle;
  FirstThunkAddr, OriginalThunkAddr: NativeUInt;
  OrigThunkVal: NativeUInt;
begin
  Result := True;
  _pImageDataDirectory := @APEHeader._pImageOptionalHeader^.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
  if _pImageDataDirectory^.Size = 0 then
    Exit(True);
  ImportDescAddr := Pointer(RVA(_pImageDataDirectory^.VirtualAddress));
  while True do
  begin
    if not ReadImportDescriptor(ImportDescAddr, _pImageImportDescriptor) then Break;
    if _pImageImportDescriptor.Name = 0 then Break;
    DllNamePtr := Pointer(RVA(_pImageImportDescriptor.Name));
    DllName := ReadStrA(DllNamePtr);
    if DllName = '' then
    begin
      ImportDescAddr := Pointer(NativeUInt(ImportDescAddr)+SizeOf(TImageImportDescriptor));
      Continue;
    end;
    hModule := LoadLibraryA(PAnsiChar(DllName));
    if hModule = 0 then
    begin
      ImportDescAddr := Pointer(NativeUInt(ImportDescAddr)+SizeOf(TImageImportDescriptor));
      Continue;
    end;
    FirstThunkAddr := RVA(_pImageImportDescriptor.FirstThunk);
    if _pImageImportDescriptor.OriginalFirstThunk = 0 then
      OriginalThunkAddr := FirstThunkAddr
    else
      OriginalThunkAddr := RVA(_pImageImportDescriptor.OriginalFirstThunk);
    while True do
    begin
      OrigThunkVal := ReadPtr(Pointer(OriginalThunkAddr));
      if OrigThunkVal = 0 then Break;
      if (OrigThunkVal and GetOrdinalFlag()) <> 0 then
      begin
        // Import by ordinal
        var OrdVal: NativeUInt := OrigThunkVal and $FFFF;
        var pFunction := GetProcAddress(hModule, MAKEINTRESOURCE(OrdVal));
        if Assigned(pFunction) then
          WritePtr(Pointer(FirstThunkAddr), NativeUInt(pFunction));
      end
      else
      begin
        // Import by name
        ImportName := ReadStrA(Pointer(RVA(OrigThunkVal)+SizeOf(Word)));
        var pFunction := GetProcAddress(hModule, PAnsiChar(ImportName));
        if Assigned(pFunction) then
          WritePtr(Pointer(FirstThunkAddr), NativeUInt(pFunction));
      end;
      Inc(OriginalThunkAddr, SizeOf(Pointer));
      Inc(FirstThunkAddr, SizeOf(Pointer));
    end;
    FreeLibrary(hModule);
    ImportDescAddr := Pointer(NativeUInt(ImportDescAddr)+SizeOf(TImageImportDescriptor));
  end;
end;

{ Remote Injection }

function TInjector.LoadLibrary(const ADLLFile: String; const ATargetProcess: String): Boolean;
var
  ProcessId: Cardinal;
begin
  ProcessId := GetProcessIdByName(ATargetProcess);
  Result := LoadLibrary(ADLLFile, ProcessId);
end;

function TInjector.LoadLibrary(const ADLLFile: String; const ATargetProcessId: Cardinal): Boolean;
var
  hProcess: THandle;
  pRemoteDllPath: Pointer;
  AThreadId: Cardinal;
  hThread: THandle;
  DllPathSize: SIZE_T;
  ExitCode: DWORD;
begin
  Result := False; // default to failure

  if not FileExists(ADLLFile) then
    raise Exception.CreateFmt('DLL file "%s" not found!', [ADLLFile]);

  hProcess := OpenProcess(PROCESS_CREATE_THREAD or PROCESS_VM_OPERATION or PROCESS_VM_WRITE, False, ATargetProcessId);
  if hProcess = 0 then
    raise EInjectorException.Create('OpenProcess');

  try
    DllPathSize := (Length(ADLLFile) + 1) * SizeOf(WideChar);
    pRemoteDllPath := VirtualAllocEx(hProcess, nil, DllPathSize, MEM_COMMIT, PAGE_READWRITE);
    if not Assigned(pRemoteDllPath) then
      raise EInjectorException.Create('VirtualAllocEx');

    try
      WriteMemory(hProcess, pRemoteDllPath, PWideChar(ADLLFile), DllPathSize);

      hThread := CreateRemoteThread(hProcess, nil, 0,
        GetProcAddress(GetModuleHandle('Kernel32.dll'), 'LoadLibraryW'),
        pRemoteDllPath, 0, AThreadId);
      if hThread = 0 then
        raise EInjectorException.Create('CreateRemoteThread');

      try
        if WaitForSingleObject(hThread, INFINITE) = WAIT_FAILED then
          raise EInjectorException.Create('WaitForSingleObject');

        // Retrieve the exit code (the HMODULE returned by LoadLibraryW)
        if not GetExitCodeThread(hThread, ExitCode) then
          raise EInjectorException.Create('GetExitCodeThread');

        Result := (ExitCode <> 0);
      finally
        CloseHandle(hThread);
      end;
    finally
      if not VirtualFreeEx(hProcess, pRemoteDllPath, 0, MEM_RELEASE) then
        raise EInjectorException.Create('VirtualFreeEx');
    end;
  finally
    CloseHandle(hProcess);
  end;
end;


function TInjector.ReflectInjectRemote(const pReflectedBase: Pointer; const ATargetProcess: String): Boolean;
var
  PID: Cardinal;
  hProcess: THandle;
  RemoteBase: Pointer;
  PEHeader: TPEHeader;
  I: Integer;
  ADelta: NativeUInt;
  AThreadId: Cardinal;
  hThread: THandle;
  NewBase: NativeUInt;
  BaseAddr: Pointer;
  BytesWritten: SIZE_T;
begin
  Result := False;
  PID := GetProcessIdByName(ATargetProcess);
  hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, PID);
  if hProcess = 0 then
    raise EInjectorException.Create('OpenProcess');
  try
    PEHeader := IdentifyPEHeader(pReflectedBase);
    if not Assigned(PEHeader.pImageBase) or (PEHeader._pImageOptionalHeader = nil) then
      raise Exception.Create('Invalid reflected module image.');
    RemoteBase := VirtualAllocEx(hProcess, nil, PEHeader._pImageOptionalHeader^.SizeOfImage, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
    if not Assigned(RemoteBase) then
      raise EInjectorException.Create('VirtualAllocEx in target process');

    // Write headers
    WriteMemory(hProcess, RemoteBase, pReflectedBase, PEHeader._pImageOptionalHeader^.SizeOfHeaders);

    // Write sections
    for I := 0 to PEHeader.SectionHeaderCount - 1 do
    begin
      var Sect := PEHeader.pSectionHeaders[I];
      if (Sect^.SizeOfRawData > 0) then
      begin
        var SectDest := Pointer(NativeUInt(RemoteBase) + Sect^.VirtualAddress);
        var SectSrc := Pointer(NativeUInt(pReflectedBase) + Sect^.PointerToRawData);
        WriteMemory(hProcess, SectDest, SectSrc, Sect^.SizeOfRawData);
      end;
    end;

    // Compute relocation delta
    ADelta := NativeUInt(RemoteBase) - PEHeader._pImageOptionalHeader^.ImageBase;

    // Remote relocations
    if ADelta <> 0 then
      if not PerformRemoteBaseRelocation(hProcess, PEHeader, RemoteBase, ADelta) then
        raise Exception.Create('Failed to perform remote base relocation.');

    // Resolve imports remotely
    if not ResolveRemoteImportTable(hProcess, PEHeader, RemoteBase) then
      raise Exception.Create('Failed to resolve imports in remote process.');

    // Update image base in remote memory using a defined constant offset
    NewBase := NativeUInt(RemoteBase);
    BaseAddr := Pointer(NativeUInt(RemoteBase) + PEHeader._pImageDosHeader._lfanew + OPTIONAL_HEADER_IMAGE_BASE_OFFSET);
    if not WriteProcessMemory(hProcess, BaseAddr, @NewBase, SizeOf(NewBase), BytesWritten) then
      raise EInjectorException.Create('WriteProcessMemory (NewImageBase)');

    // Create remote thread to execute entry point
    var EntryPoint := Pointer(NativeUInt(RemoteBase) + PEHeader._pImageOptionalHeader^.AddressOfEntryPoint);
    hThread := CreateRemoteThread(hProcess, nil, 0, EntryPoint, nil, 0, AThreadId);
    if hThread = 0 then
      raise EInjectorException.Create('CreateRemoteThread (entry point)');
    try
      if WaitForSingleObject(hThread, INFINITE) = WAIT_FAILED then
        raise EInjectorException.Create('WaitForSingleObject (entry point thread)');
    finally
      CloseHandle(hThread);
    end;
    Result := True;
  finally
    CloseHandle(hProcess);
  end;
end;

{ ShellCode Loader Injection }

function TInjector.InjectViaShellLoader(hProcess: THandle; const ADLLFile: String): Boolean;
var
  pPool: Pointer;
  pRemoteDllPath: Pointer;
  AThreadId: Cardinal;
  hThread: THandle;
  DllPathSize: SIZE_T;
  Shellcode: array[0..31] of Byte;
begin
  Result := False;
  if not FileExists(ADLLFile) then
    raise Exception.CreateFmt('DLL file "%s" not found!', [ADLLFile]);

  // Allocate memory pool in the target process
  DllPathSize := (Length(ADLLFile) + 1) * SizeOf(WideChar);
  pPool := VirtualAllocEx(hProcess, nil, DllPathSize + 1024, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
  if not Assigned(pPool) then
    raise EInjectorException.Create('VirtualAllocEx (ShellLoader)');

  try
    // Write the DLL path into the allocated pool
    pRemoteDllPath := Pointer(NativeUInt(pPool) + 512); // Offset for shellcode
    WriteMemory(hProcess, pRemoteDllPath, PWideChar(ADLLFile), DllPathSize);

    // Initialize the shellcode
    Shellcode[0] := $68; // push
    PCardinal(@Shellcode[1])^ := NativeUInt(pRemoteDllPath); // Address of pRemoteDllPath
    Shellcode[5] := $B8; // mov eax
    PCardinal(@Shellcode[6])^ := NativeUInt(GetProcAddress(GetModuleHandle('Kernel32.dll'), 'LoadLibraryW')); // Address of LoadLibraryW
    Shellcode[10] := $FF; // call
    Shellcode[11] := $D0; // eax
    Shellcode[12] := $C3; // ret

    // Write shellcode to the allocated pool
    WriteMemory(hProcess, pPool, @Shellcode, SizeOf(Shellcode));

    // Execute the shellcode
    hThread := CreateRemoteThread(hProcess, nil, 0, pPool, nil, 0, AThreadId);
    if hThread = 0 then
      raise EInjectorException.Create('CreateRemoteThread (ShellLoader)');
    try
      if WaitForSingleObject(hThread, INFINITE) = WAIT_FAILED then
        raise EInjectorException.Create('WaitForSingleObject (ShellLoader)');
    finally
      CloseHandle(hThread);
    end;
    Result := True;
  finally
    if not VirtualFreeEx(hProcess, pPool, 0, MEM_RELEASE) then
      raise EInjectorException.Create('VirtualFreeEx (ShellLoader)');
  end;
end;

procedure TInjector.InjectShellLoader(const ADLLFile: String; const ATargetProcess: String);
var
  PID: Cardinal;
  hProcess: THandle;
begin
  PID := GetProcessIdByName(ATargetProcess);
  hProcess := OpenProcess(PROCESS_ALL_ACCESS, False, PID);
  if hProcess = 0 then
    raise EInjectorException.Create('OpenProcess');
  try
    if not InjectViaShellLoader(hProcess, ADLLFile) then
      raise Exception.Create('ShellLoader injection failed.');
  finally
    CloseHandle(hProcess);
  end;
end;

end.
