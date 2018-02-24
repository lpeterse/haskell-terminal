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

int hs_get_console_input_mode_desired(void);
int hs_set_console_input_mode(int);
int hs_get_console_output_mode_desired(void);
int hs_set_console_output_mode(int);

DWORD hs_wait_console_input(DWORD);
int hs_read_console_input(INPUT_RECORD*);

#endif