#include <stdio.h>  // for fileno()
#include <unistd.h> // for isatty()

_Bool
is_a_tty ()
{
  return isatty (fileno (stdout));
}
