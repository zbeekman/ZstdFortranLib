program error_stack_test
  !! category: testing
  !! author: Izaak Beekman
  !!
  !! Unit test for the c_env_interfaces module

  use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit
  use zsfl_testing, only: unit_test_t
  use zsfl_error_stack
  implicit none

  character(len=*), parameter :: file = &
    _FILE_

  integer :: tty_dims(2) = [-1, -1], i
  type(unit_test_t) :: test

  call test%initialize(file)

  do i = 1,2
     test = .not. tty_dims(i) >= 0
  end do


  tty_dims = get_terminal_dims()

  do i = 1,2
     test = tty_dims(i) >= 0
  end do

  print*, tty_dims

  call print_terminal_dims()
  call print_terminal_dims(stdout)
  call print_terminal_dims(stderr)

  call test%report_status()
end program
