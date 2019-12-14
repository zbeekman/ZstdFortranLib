#include <sys/ioctl.h> /* ioctl, TIOCGWINSZ */
#include <stdio.h>
#include <unistd.h>

int
get_connected_tty_lun (void);

int
get_tty_rows ()
{
  struct winsize win;
  int flun = get_connected_tty_lun ();

  if (flun < 0)
    return 0;

  /* Get window size of terminal. */
  ioctl (flun, TIOCGWINSZ, &win);
  return (int) win.ws_row;
}

int
get_tty_cols ()
{
  struct winsize win;
  int flun = get_connected_tty_lun ();

  if (flun < 0)
    return 0;

  /* Get window size of terminal. */
  ioctl (flun, TIOCGWINSZ, &win);
  return (int) win.ws_col;
}
