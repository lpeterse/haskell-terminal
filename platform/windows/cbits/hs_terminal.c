#include "hs_terminal.h"

BOOL hs_get_console_input_mode(LPDWORD mode) {
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);
    return GetConsoleMode(h, mode);
}

BOOL hs_set_console_input_mode(DWORD mode) {
    HANDLE h = GetStdHandle(STD_INPUT_HANDLE);
    return SetConsoleMode(h, mode);
}

BOOL hs_get_console_output_mode(LPDWORD mode) {
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    return GetConsoleMode(h, mode);
}

BOOL hs_set_console_output_mode(DWORD mode) {
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    return SetConsoleMode(h, mode);
}

BOOL hs_read_console_input(INPUT_RECORD *record) {
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

BOOL hs_get_console_screen_buffer_info(CONSOLE_SCREEN_BUFFER_INFO *csbi) {
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    return GetConsoleScreenBufferInfo(h, csbi);
}

BOOL hs_write_console(VOID *lpBuffer, DWORD nNumberOfCharsToWrite, DWORD *lpNumberOfCharsWritten) {
    HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE);
    return WriteConsoleW(h, lpBuffer, nNumberOfCharsToWrite, lpNumberOfCharsWritten, NULL);
}
