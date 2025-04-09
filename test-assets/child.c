#include <stdio.h>

int wmain(int argc, wchar_t** argv)
{
    for (int arg = 1; arg < argc; ++arg) {
        wprintf(L"%ls\n", argv[arg]);
    }
    return 0;
}
