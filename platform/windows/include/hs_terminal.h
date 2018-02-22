#define DEFINE_CONSOLEV2_PROPERTIES

#include <windows.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef MOUSE_HWHEELED
#define MOUSE_HWHEELED 0x0008
#endif

int hs_get_console_input_mode_desired(void);
int hs_set_console_input_mode(int);
int hs_get_console_output_mode_desired(void);
int hs_set_console_output_mode(int);

int hs_read_console_input(INPUT_RECORD*);
