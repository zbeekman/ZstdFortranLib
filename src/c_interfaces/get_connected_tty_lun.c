#include <stdio.h>
#include <unistd.h>

int
get_connected_tty_lun ()
{
  int candidate_fds[3] = {STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO};
  int found_tty = -1;
  int i; //! OCLINT(i ok variable name for loop index)

  for (i = 0; i < 3; i++)
    {
      found_tty = candidate_fds[i];
      if (isatty (found_tty))
	return found_tty;
    }
  // No tty found, return -1
  return -1;
}
