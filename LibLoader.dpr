library LibLoader;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters.

  Important note about VCL usage: when this DLL will be implicitly
  loaded and this DLL uses TWicImage / TImageCollection created in
  any unit initialization section, then Vcl.WicImageInit must be
  included into your library's USES clause. }

uses
  Windows,
  System.SysUtils,
  System.Classes,
  System.AnsiStrings,
  Loader in 'Loader.pas',
  Log in 'Log.pas';

type
  TOrgFuncs = record
  case Integer of
    0: (Base: FARPROC;);
    1: (Arr: array [0..7] of FARPROC;);
  end;

const
  SIZE_OF_FUNC = SizeOf(FARPROC);

var
  hl: HINST;
  OrgFuncs: TOrgFuncs;

{$R *.res}

procedure __DLLProc(Reason: Integer);
begin
  case Reason of
    DLL_PROCESS_ATTACH:
    begin
      hl := LoadLibrary('nps64.dat');
      if hl = 0 then Exit;

      OrgFuncs.Arr[0] :=  GetProcAddress(hl, 'AddSurveyParameterA');
      OrgFuncs.Arr[1] :=  GetProcAddress(hl, 'AddSurveyParameterW');
      OrgFuncs.Arr[2] :=  GetProcAddress(hl, 'GetLastErrorCode');
      OrgFuncs.Arr[3] :=  GetProcAddress(hl, 'GetLastErrorMsg');
      OrgFuncs.Arr[4] :=  GetProcAddress(hl, 'OpenSurveyWindowA');
      OrgFuncs.Arr[5] :=  GetProcAddress(hl, 'OpenSurveyWindowW');
      OrgFuncs.Arr[6] :=  GetProcAddress(hl, 'SetDebugMode');
      OrgFuncs.Arr[7] :=  GetProcAddress(hl, 'SetRunMode');

    end;

    DLL_PROCESS_DETACH:
    begin
      FreeLibrary(hl);
    end;
  end;
end;

// AddSurveyParameterA
procedure __E__0__();
asm
  jmp [OrgFuncs.Base + SIZE_OF_FUNC * 0]
end;


// AddSurveyParameterW
procedure __E__1__();
asm
  jmp [OrgFuncs.Base + SIZE_OF_FUNC * 1]
end;


// GetLastErrorCode
procedure __E__2__();
asm
  jmp [OrgFuncs.Base + SIZE_OF_FUNC * 2]
end;


// GetLastErrorMsg
procedure __E__3__();
asm
  jmp [OrgFuncs.Base + SIZE_OF_FUNC * 3]
end;


// OpenSurveyWindowA
procedure __E__4__();
asm
  jmp [OrgFuncs.Base + SIZE_OF_FUNC * 4]
end;


// OpenSurveyWindowW
procedure __E__5__();
asm
  jmp [OrgFuncs.Base + SIZE_OF_FUNC * 5]
end;


// SetDebugMode
procedure __E__6__();
asm
  jmp [OrgFuncs.Base + SIZE_OF_FUNC * 6]
end;


// SetRunMode
procedure __E__7__();
asm
  jmp [OrgFuncs.Base + SIZE_OF_FUNC * 7]
end;


//-------- Exports
exports
  __E__0__ index 1 name 'AddSurveyParameterA',
  __E__1__ index 2 name 'AddSurveyParameterW',
  __E__2__ index 3 name 'GetLastErrorCode',
  __E__3__ index 4 name 'GetLastErrorMsg',
  __E__4__ index 5 name 'OpenSurveyWindowA',
  __E__5__ index 6 name 'OpenSurveyWindowW',
  __E__6__ index 7 name 'SetDebugMode',
  __E__7__ index 8 name 'SetRunMode';

begin
  DllProc := __DLLProc;
  __DLLProc(DLL_PROCESS_ATTACH);
end.
