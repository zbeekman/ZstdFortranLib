program test_env_interface
  !! Unit test for the environment interface & OS object
  use zsfl_env_interface, only: OS, operator(.env.), operator(.envExists.)
  use zsfl_strings, only: &
    gsub, ascii_k, utf8_k, sub, split, join, to_s, to_i, to_l, to_r, init_float_fmt, nl, &
    colorize => maybe_colorize, use_color, operator(//)
  implicit none

  logical :: test_passed = .true.

  call OS%init()
  call OS%get_info()

  test_passed = test_passed .and. OS%env_exists("USER")
  test_passed = test_passed .and. OS%env_exists("HOME")
  test_passed = test_passed .and. .envExists. "PATH"
  test_passed = test_passed .and. len(OS%env("HOME")) >= 1
  test_passed = test_passed .and. len( .env. "USER" ) >= 1

  if(test_passed) then
    write(*,'(A)') "Test passed."
  else
    write(*,'(A)') "Test failed."
  endif
end program
