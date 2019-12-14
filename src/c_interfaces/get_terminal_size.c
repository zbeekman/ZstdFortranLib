#include <sys/ioctl.h> /* ioctl, TIOCGWINSZ */
#include <err.h>       /* err */
#include <fcntl.h>     /* open */
#include <stdio.h>     /* printf */
#include <unistd.h>    /* close */

int n_cols, n_rows;

int
get_tty_rows ()
{
  struct winsize win;
  int flun;

  char *tty_name = ttyname (STDIN_FILENO);
  /* Open the controlling terminal. */
  flun = open (tty_name, O_RDWR);
  if (flun < 0)
    err (1, "%s", tty_name);

  /* Get window size of terminal. */
  if (ioctl (flun, TIOCGWINSZ, &win) < 0) //! OCLINT(Not a bitwise &)
    err (1, "%s", tty_name);

  n_rows = (int) win.ws_row;
  close (flun);
  return n_rows;
}

int
get_tty_cols ()
{
  struct winsize win;
  int flun;

  char *tty_name = ttyname (STDIN_FILENO);
  /* Open the controlling terminal. */
  flun = open (tty_name, O_RDWR);
  if (flun < 0)
    err (1, "%s", tty_name);

  /* Get window size of terminal. */
  if (ioctl (flun, TIOCGWINSZ, &win) < 0) //! OCLINT(Not a bitwise &)
    err (1, "%s", tty_name);

  n_cols = (int) win.ws_col;
  close (flun);
  return n_cols;
}
