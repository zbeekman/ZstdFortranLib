module zsfl_c_system_interface

  implicit none

  ! get_terminal_size.c
  interface
    integer(c_int) function get_tty_rows() bind(C)
       use, intrinsic :: iso_c_binding
       implicit none
     end function
     integer(c_int) function get_tty_cols() bind(C)
       use, intrinsic :: iso_c_binding
       implicit none
     end function
  end interface

  ! is_a_tty.c
  interface
    logical(c_bool) function is_a_tty() bind(C)
      use, intrinsic :: iso_c_binding
    end function
  end interface

end module
