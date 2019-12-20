program env_interface_test
  !! category: testing
  !! author: Izaak Beekman
  !!
  !! Unit test for the environment interface & OS object

  use zsfl_testing, only: unit_test_t
  use zsfl_env_interface, only: OS, operator(.env.), operator(.envExists.)
  use zsfl_strings, only: &
    gsub, ascii_k, utf8_k, sub, split, join, to_s, to_i, to_l, to_r, init_float_fmt, nl, &
    colorize => maybe_colorize, use_color, operator(//)
  implicit none

  character(len=*), parameter :: file = &
    _FILE_

  type(unit_test_t) :: test

  call test%initialize(file)

  call OS%init()
  call OS%get_info()

  test = OS%env_exists("USER")
  test = OS%env_exists("HOME")
  test = .envExists. "PATH"
  test = len(OS%env("HOME")) >= 1
  test = len( .env. "USER" ) >= 1

  call test%report_status()
end program
