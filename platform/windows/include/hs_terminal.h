#ifndef HS_TERMINAL_H
#define HS_TERMINAL_H

#define DEFINE_CONSOLEV2_PROPERTIES

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

#ifndef MOUSE_HWHEELED
#define MOUSE_HWHEELED                     0x0008
#endif

BOOL  hs_get_console_input_mode(LPDWORD);
BOOL  hs_set_console_input_mode(DWORD);
BOOL  hs_get_console_output_mode(LPDWORD);
BOOL  hs_set_console_output_mode(DWORD);

DWORD hs_wait_console_input(DWORD);
BOOL  hs_read_console_input(INPUT_RECORD*);
BOOL  hs_write_console(VOID*, DWORD, DWORD*);

BOOL  hs_get_console_screen_buffer_info(CONSOLE_SCREEN_BUFFER_INFO*);

#endif