# process-escape-windows

This repo contains a new implementation of argument escaping for use with the [CreateProcess](https://learn.microsoft.com/en-us/windows/win32/api/processthreadsapi/nf-processthreadsapi-createprocessw) call on Windows. It is intended to replace the current implementation in Haskell's `System.Process` library.

The main code has no dependencies and can be found in [Lib.hs](./src/Lib.hs). It consists of two functions, `escapeCreateProcessArg0` and `escapeCreateProcessArg`. The former is for escaping the first argument (i.e. the executable path) passed as part of a `CreateProcess` command line string, and the latter is for all subsequent arguments. They are different functions because Windows treats the first argument differently.

This repo contains regular tests and QuickCheck tests to verify correctness. The QuickCheck tests use FFI to call the `CommandLineToArgvW` function from the Windows C API, which parses the arguments the way Windows executables do. This allows us to check that our escaped arguments are always interpreted by a Windows executable the way we intended.

To run the tests (on a Windows machine):

``` shell
stack test --ta=--tui
```
