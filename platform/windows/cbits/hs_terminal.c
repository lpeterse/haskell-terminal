#include <windows.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef ENABLE_VIRTUAL_TERMINAL_PROCESSING
#define ENABLE_VIRTUAL_TERMINAL_PROCESSING 0x0004
#endif

#ifndef ENABLE_PROCESSED_INPUT
#define ENABLE_PROCESSED_INPUT             0x0001
#endif

#ifndef ENABLE_LINE_INPUT
#define ENABLE_LINE_INPUT                  0x0002
#endif

#ifndef ENABLE_ECHO_INPUT
#define ENABLE_ECHO_INPUT                  0x0004
#endif

#ifndef ENABLE_VIRTUAL_TERMINAL_INPUT
#define ENABLE_VIRTUAL_TERMINAL_INPUT      0x0200
#endif

int hs_get_console_input_mode_desired(void) {
    int mode = 0;
    mode = mode | ENABLE_VIRTUAL_TERMINAL_INPUT;
    return mode;
}

int hs_get_console_output_mode_desired(void) {
    int mode = 0;
    mode = mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    return mode;
}

int hs_set_console_input_mode(int mode) {
    DWORD oldMode = 0;
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);

    if (h == INVALID_HANDLE_VALUE) {
        return -1;
    }

    if (!GetConsoleMode(h, &oldMode)) {
        return -1;
    }

    DWORD newMode = oldMode;
    newMode = mode & ENABLE_PROCESSED_INPUT        ? newMode | ENABLE_PROCESSED_INPUT        : newMode & ~ENABLE_PROCESSED_INPUT;
    newMode = mode & ENABLE_LINE_INPUT             ? newMode | ENABLE_LINE_INPUT             : newMode & ~ENABLE_LINE_INPUT;
    newMode = mode & ENABLE_ECHO_INPUT             ? newMode | ENABLE_ECHO_INPUT             : newMode & ~ENABLE_ECHO_INPUT;
    newMode = mode & ENABLE_VIRTUAL_TERMINAL_INPUT ? newMode | ENABLE_VIRTUAL_TERMINAL_INPUT : newMode & ~ENABLE_VIRTUAL_TERMINAL_INPUT;

    if (!SetConsoleMode(h, newMode)) {
        return -1;
    }

    return oldMode & ( ENABLE_PROCESSED_INPUT
                     | ENABLE_LINE_INPUT
                     | ENABLE_ECHO_INPUT
                     | ENABLE_VIRTUAL_TERMINAL_INPUT
                     );
}

int hs_set_console_output_mode(int mode) {
    DWORD oldMode = 0;
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);

    if (h == INVALID_HANDLE_VALUE) {
        return -1;
    }

    if (!GetConsoleMode(h, &oldMode)) {
        return -1;
    }

    DWORD newMode = oldMode;
    newMode = mode & ENABLE_VIRTUAL_TERMINAL_PROCESSING ? newMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING : newMode & ~ENABLE_VIRTUAL_TERMINAL_PROCESSING;

    if (!SetConsoleMode(h, newMode)) {
        return -1;
    }

    return oldMode & ENABLE_VIRTUAL_TERMINAL_PROCESSING;
}

int hs_get_console_winsize(int *rows, int *cols) {
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    if (!GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi)) {
        return -1;
    }
    *rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
    *cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    return 0;
}
