#include <sys/ioctl.h>
#include <stdio.h>
#include <termios.h>

void hs_get_winsize(unsigned short *rows, unsigned short *cols);