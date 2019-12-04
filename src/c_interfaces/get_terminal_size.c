#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>

int n_cols, n_rows ;

int get_tty_rows(){
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  n_rows = (int)w.ws_row;
  return n_rows;
}

int get_tty_cols(){
  struct winsize w;
  ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);
  n_cols = (int)w.ws_col;
  return n_cols;
}
