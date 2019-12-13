program test_c_interfaces
  use zsfl_c_system_interface, only: is_a_tty, get_tty_rows, get_tty_cols
  implicit none
  integer :: rows, cols
  logical :: passing = .true.

  rows = get_tty_rows()
  cols = get_tty_cols()
  write(*,*) "Rows: ", rows
  write(*,*) "Cols: ", cols
  passing = passing .and. rows >= 0 .and. cols >= 0

  if(passing) then
    write(*,'(A)') "Test passed."
  else
    write(*,'(A)') "Test failed."
  endif
end program
