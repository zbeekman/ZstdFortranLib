program c_interfaces_test
  !! category: testing
  !! author: Izaak Beekman
  !!
  !! Unit test for the c_env_interfaces module

  use zsfl_testing, only: unit_test_t
  use zsfl_c_system_interface, only: is_a_tty, get_tty_rows, get_tty_cols
  implicit none

  character(len=*), parameter :: file = &
    _FILE_
  integer :: rows, cols
  type(unit_test_t) :: test

  call test%initialize(file)

  rows = get_tty_rows()
  cols = get_tty_cols()
  write(*,*) "Rows: ", rows
  write(*,*) "Cols: ", cols
  test = rows >= 0
  test = cols >= 0
  write(*,*) "Is a tty? : ", is_a_tty()

  call test%report_status()
end program
