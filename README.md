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

## Usage

LibLoader is configured via a single INI file (`Loader.cfg`). See the configuration file documentation for details on setting options for module loading, delayed tasks, injection methods, and more.
