program main
  use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit
  use zsfl_error_stack

  implicit none
  logical :: test_failed = .false.

  integer :: tty_dims(2) = [-1, -1], i

  do i = 1,2
     test_failed = test_failed .or. tty_dims(i) >= 0
  end do


  tty_dims = get_terminal_dims()

  do i = 1,2
     test_failed = test_failed .or. tty_dims(i) < 0
  end do

  print*, tty_dims

  call print_terminal_dims()
  call print_terminal_dims(stdout)
  call print_terminal_dims(stderr)

  if ( test_failed ) then
     stop 1
  else
     stop 0
  end if
end program
