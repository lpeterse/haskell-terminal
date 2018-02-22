#include <sys/ioctl.h>
#include <stdio.h>
#include <termios.h>

int hs_get_winsize(int fd, unsigned short *rows, unsigned short *cols) {

    struct winsize w;
    if (ioctl(fd, TIOCGWINSZ, &w) > -1) {
        *rows = w.ws_row;
        *cols = w.ws_col;
        return 0;
    } else{
        return -1;
    }
}