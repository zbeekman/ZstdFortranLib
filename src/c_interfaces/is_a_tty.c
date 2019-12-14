#include <stdio.h>  // for fileno()
#include <unistd.h> // for isatty()

int
get_connected_tty_lun (void);

_Bool
is_a_tty ()
{
  int tty_fd = -1;
  tty_fd = get_connected_tty_lun ();
  return isatty (tty_fd);
}
