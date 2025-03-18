# LibLoader v3

LibLoader is a proxy DLL that supports chain-loading additional DLLs for games, providing a robust and flexible module loading mechanism.

## Key Features

- **Multiple Mod Directories:**  
  Search for modules in multiple directories, even outside the game directory.

- **Recursive Directory Search:**  
  Recursively searches each specified directory for the required DLL files.

- **Fallback Mechanism:**  
  If the recursive search fails, the loader automatically falls back to the host application's directory.

- **Delayed Module Loading:**  
  Supports delayed loading of modules based on configurable delays, or waits for a specific process or module before loading.

- **DLL Injection Support:**  
  In addition to the standard LoadLibrary method, LibLoader offers DLL injection, allowing you to inject DLLs into a target process when needed.

- **Advanced Module Selection:**  
  When multiple versions of a module exist, the loader chooses the best candidate by comparing version numbers and file dates.

- **Enhanced Logging and Performance:**  
  Detailed logging with timing metrics, thread identifiers, and a directory caching mechanism helps optimize performance and troubleshooting.

- **Configurable via INI File:**  
  Fully customizable configuration file (`Loader.cfg`) to control which modules to load, search directories, delays, injection methods, and more.

## Usage Instructions

### Configuration File (Loader.cfg)

The configuration file remains the heart of the loader’s setup, and this version supports additional options for delayed loading, injection methods, and more granular control over the loading process. Below is a detailed explanation of each section and its parameters:

#### [Loader] Section:
- **Enabled:**  
  Set to `1` to enable the chain loading process; set to `0` to disable it.
- **Files:**  
  A comma-separated list of DLLs to load. List the filenames (e.g., `MabiWnd.dll, BCGv2.dll`). These are the modules that the loader will attempt to find and load.
- **ModFolders:**  
  A comma-separated list of directories where the loader will recursively search (including subdirectories) for the DLLs specified in the Files list. If a module isn’t found in these folders, the loader will automatically check the host application’s directory.

#### [Debug] Section:
- **Enabled:**  
  Toggle detailed debug output by setting this to `1` (on) or `0` (off). When enabled, additional debug messages are logged.
- **Mode:**  
  Specifies the debug output mode. For example, `1` outputs log messages only, while `2` outputs log messages along with additional debug information (e.g., via DebugView).

#### [Log] Section:
- **Append:**  
  Determines the log file write mode. Setting it to `1` will append new logs to the existing file, while `0` will overwrite the log file on startup.

#### Module-Specific Sections:
Each module can have its own section to define specific loading behavior. The following parameters are available:

- **Delay:**  
  The time (in milliseconds) to wait before attempting to load the module. This allows you to postpone the loading of certain modules until other conditions are met.
- **DelayPosition:**  
  Defines when the delay should be applied relative to other waiting conditions.  
  **Options:**  
  - **None:** The delay is applied immediately (or not used at all).  
  - **Before:** Apply the delay before checking for other waiting conditions (e.g., waiting for a process or another module).  
  - **After:** Apply the delay after the waiting conditions have been met.
- **WaitForProcess:**  
  The name of a process that the loader should wait for before loading the module. The loader will continuously check if this process is running before proceeding.
- **WaitForModule:**  
  The name of another module (DLL) that must be loaded before this module is loaded. This helps enforce dependency order.
- **Method:**  
  Specifies the method to use for loading the DLL:
  - `1`: Normal LoadLibrary (standard dynamic loading).
  - `2`: LoadLibrary injection (which attempts to load the DLL into a target process).
  - Additional options for reflective injection (into the current or a target process) may be defined as needed.
- **Target:**  
  If using an injection method (Method = 2 or similar), this parameter specifies the target process name for injection.

## Updated Configuration File Example

Below is an example configuration file with inline comments explaining each parameter. *(Note: Adjust file names and paths to suit your environment.)*

```ini
[Loader]
; Enable Lib-Loader to run the chain loading process.
; 1 = On, 0 = Off
Enabled = 1

; Specify DLL libraries to find and load.
; Example: Files = MyModule.dll, ExtraModule.dll, AnotherMod.dll
Files = MyModule.dll, ExtraModule.dll

; Specify separate mod directories to recursively search (including sub-dirs)
; for the above-defined files. If not found in these directories,
; the loader will default to the application directory.
; Example: ModFolders = C:\Projects\Mods, D:\Builds\DLLs
ModFolders = C:\Projects\Mods, D:\Builds\DLLs

[Debug]
; Enables detailed debug output to the log file.
Enabled = 1
; Debug output mode: 1 = Log only, 2 = Log + DebugView
Mode = 1

[Log]
; Set the write mode for the log file.
; 0 = Overwrite the log on startup, 1 = Append to the existing log
Append = 0

;------------------------------------------------------------------------------------------------------------------------------
; Module-Specific Options: Configure individual DLL load options below.
;------------------------------------------------------------------------------------------------------------------------------

[MyModule.dll]
; Delay in milliseconds before attempting to load this module.
; This delay can be used to postpone the load operation.
Delay = 10000

; Determines when the delay is applied relative to other waiting conditions.
; Options:
;   None   - No special timing; delay is applied immediately (or not used).
;   Before - Apply the delay before checking for WaitForProcess/WaitForModule conditions.
;   After  - Apply the delay after the waiting conditions are met.
DelayPosition = None

; Specify a process name that must be running before attempting to load this module.
WaitForProcess = SampleProcess.exe

; Specify an alternative method for loading the DLL.
; Options:
;   1 = Normal LoadLibrary
;   2 = LoadLibrary injection with a target process
;   (Additional methods can be defined as needed.)
Method = 2

; If using an injection method, specify the target process name here.
Target = SampleProcess.exe

[ExtraModule.dll]
; Delay (in milliseconds) before attempting to load this module.
Delay = 5000

; Wait for another module to load before proceeding.
WaitForModule = MyModule.dll

; Use the normal LoadLibrary method for this module.
Method = 1