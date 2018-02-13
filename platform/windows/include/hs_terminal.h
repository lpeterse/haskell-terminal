#define DEFINE_CONSOLEV2_PROPERTIES

#include <windows.h>
#include <wchar.h>
#include <stdlib.h>
#include <stdio.h>

int hs_get_console_input_mode_desired(void);
int hs_set_console_input_mode(int);
int hs_get_console_output_mode_desired(void);
int hs_set_console_output_mode(int);
