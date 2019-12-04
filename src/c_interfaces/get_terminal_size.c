#include <stdio.h>
#include <sys/ioctl.h>
#include <unistd.h>

int n_cols, n_rows;

int
get_tty_rows ()
{
  struct winsize win;
  // NOLINTNEXTLINE(hicpp-signed-bitwise)
  ioctl (STDOUT_FILENO, TIOCGWINSZ, &win);
  n_rows = (int) win.ws_row;
  return n_rows;
}

int
get_tty_cols ()
{
  struct winsize win;
  // NOLINTNEXTLINE(hicpp-signed-bitwise)
  ioctl (STDOUT_FILENO, TIOCGWINSZ, &win);
  n_cols = (int) win.ws_col;
  return n_cols;
}
