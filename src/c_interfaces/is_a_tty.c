#include <unistd.h>   // for isatty()
#include <stdio.h>    // for fileno()

_Bool is_a_tty()
{
  return isatty(fileno(stdout));
}
