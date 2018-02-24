#include <windows.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>



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
    newMode = mode & ~ENABLE_QUICK_EDIT_MODE;

    if (!SetConsoleMode(h, newMode | ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT | ENABLE_EXTENDED_FLAGS )) {
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

int hs_read_console_input(INPUT_RECORD *record) {
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);
    INPUT_RECORD irInBuf[1];
    DWORD recordsRead = 0;

    if (!ReadConsoleInputW(h, record, 1, &recordsRead)) {
        return -1;
    }
    if (!recordsRead) {
        return -1;
    }
    return 0;
}

DWORD hs_wait_console_input(DWORD timeoutMillis) {
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);
    return WaitForSingleObject(h, timeoutMillis);
}
