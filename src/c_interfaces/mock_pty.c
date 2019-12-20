/*
 *  Connect program passed as the 1st argument to a pty for CI testing where a
 * TTY is not connected.
 */
#include <signal.h>
#include <stdlib.h>
#include <sysexits.h>
#include <unistd.h>
#include <stdio.h>
#include <util.h>

pid_t child = 0;

void
sighandler (int signum)
{
  if (child > 0)
    {
      killpg (child, signum);
      exit (signum);
    }
}

int
forward_output (int parent_pid)
{
  // Forward output to parent process
  const int buf_size = 1024;
  char buf[buf_size];
  fd_set fds;
  ssize_t bytes_read;

  // forward the output continuously
  while (1)
    {
      // NOLINTNEXTLINE
      FD_ZERO (&fds);
      FD_SET (parent_pid, &fds);

      if (select (parent_pid + 1, &fds, NULL, NULL, NULL) > 0
	  && FD_ISSET (parent_pid, &fds))
	{
	  bytes_read = read (parent_pid, buf, buf_size);
	  if (bytes_read <= 0)
	    {
	      return EXIT_SUCCESS;
	    }

	  if (write (STDOUT_FILENO, buf, bytes_read) != bytes_read)
	    {
	      perror ("failed to write to stdout");
	      return EX_OSERR;
	    }
	}
    }
}

int
main (int argc, char *argv[])
{
  // Run a command in a pty.
  // Usage: /path/to/this/binary command to run
  if (argc < 2)
    {
      return EX_USAGE;
    }

  int master;
  child = forkpty (&master, NULL, NULL, NULL);

  if (child == -1)
    {
      perror ("failed to fork pty");
      return EX_OSERR;
    }

  if (child == 0)
    {
      // we're in the child process, so replace it with the command
      execvp (argv[1], argv + 1);
      perror ("failed to execute command");
      return EX_OSERR;
    }

  // trap kill signals and forward them to child process
  signal (SIGHUP, sighandler);
  signal (SIGINT, sighandler);
  signal (SIGTERM, sighandler);

  return forward_output (master);
}
