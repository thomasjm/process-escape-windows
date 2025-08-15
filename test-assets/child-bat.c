#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <windows.h>

int wmain(int argc, wchar_t** argv)
{
    // Important: Haskell's System.Process functions like readCreateProcessWithExitCode
    // expect UTF8, so we have to set stdout to that mode
    _setmode(_fileno(stdout), _O_U8TEXT);

    if (argc < 2) {
        fwprintf(stderr, L"Usage: child-bat.exe <bat-file-path> [args...]\n");
        return 1;
    }

    // Build command line: bat-file-path arg1 arg2 ...
    wchar_t cmdline[32768]; // Windows command line limit
    wcscpy_s(cmdline, sizeof(cmdline)/sizeof(wchar_t), argv[1]); // bat file path

    // Concatenate remaining arguments with spaces
    for (int arg = 2; arg < argc; ++arg) {
        wcscat_s(cmdline, sizeof(cmdline)/sizeof(wchar_t), L" ");
        wcscat_s(cmdline, sizeof(cmdline)/sizeof(wchar_t), argv[arg]);
    }

    fwprintf(stderr, L"cmdline: %s\n", cmdline);

    // Create process to run the command
    STARTUPINFOW si;
    PROCESS_INFORMATION pi;

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);
    si.dwFlags = STARTF_USESTDHANDLES;
    si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    ZeroMemory(&pi, sizeof(pi));

    if (!CreateProcessW(NULL, cmdline, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        fwprintf(stderr, L"CreateProcess failed (%d)\n", GetLastError());
        return 1;
    }

    // Wait for the child process to complete
    WaitForSingleObject(pi.hProcess, INFINITE);

    // Get exit code
    DWORD exitCode;
    GetExitCodeProcess(pi.hProcess, &exitCode);

    // Close process and thread handles
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);

    return (int)exitCode;
}
