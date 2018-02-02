#include <windows.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif

int hs_set_vt_mode(int enable) {
    DWORD oldMode = 0;
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);

    if (hOut == INVALID_HANDLE_VALUE) {
        return -1;
    }

    if (!GetConsoleMode(hOut, &oldMode)) {
        return -1;
    }

    DWORD newMode = enable
      ? oldMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING
      : oldMode & ~ENABLE_VIRTUAL_TERMINAL_PROCESSING;

    if (!SetConsoleMode(hOut, newMode)) {
        return -1;
    }

    return (oldMode & ENABLE_VIRTUAL_TERMINAL_PROCESSING) ? 1 : 0;
}