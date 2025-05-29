#include <fcntl.h>
#include <stdio.h>

int wmain(int argc, wchar_t** argv)
{
    _setmode(_fileno(stdout), _O_U8TEXT);

    for (int arg = 1; arg < argc; ++arg) {
        wprintf(L"%ls\n", argv[arg]);
    }
    return 0;
}
