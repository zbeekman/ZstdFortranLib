module zsfl_error_stack

  use, intrinsic :: ISO_FORTRAN_ENV, only: int8, stdout => output_unit, stderr => error_unit
  implicit none

  private
  public :: print_terminal_dims, get_terminal_dims

  integer, parameter :: min_width = 72, max_width = 132
  character(len=*), parameter :: horizontal_ws = achar(9) // achar(32) ! TAB and Space
  character(len=*), parameter :: vertical_breaks =  &
       achar(10) // & ! new line, line-feed
       achar(11) // & ! vertical tab
       achar(12) // & ! form feed
       achar(13)      ! carriage return
  character(len=*), parameter :: splittable_with_indent = '/;'

  integer, volatile :: n_rows = 0, n_columns = 0
  logical :: page_size_set = .false.

  type message
     private
     logical :: initialized = .false.
     integer :: line_len = 0
     integer :: page_len = 0
     character(len=:), allocatable :: page(:)
   contains
     ! procedure :: write => write_string
     ! procedure :: append => append_string
     ! procedure :: clear => del_message
     ! procedure :: get => get_all_as_string
  end type message

  type error_t
     logical :: is_raised = .false.
     integer :: type_of_error = 0
  end type error_t

contains
  subroutine initialize_error_stack_module()
!    n_rows = get_tty_rows()
!    n_columns = get_tty_cols()
    page_size_set = .true.
  end subroutine

  subroutine print_terminal_dims(unit)
    integer, intent(in), optional :: unit
    integer :: lun
    if ( .not. page_size_set ) then
       call initialize_error_stack_module()
    end if

    lun = stdout
    if ( present(unit) ) then
       write(unit,'(A, I0, A, I0, A)') "tty is ", n_columns, " columns by ", n_rows, " rows."
    else
       write(*,'(A, I0, A, I0, A)') "tty is ", n_columns, " columns by ", n_rows, " rows."
    end if
  end subroutine

  function get_terminal_dims() result(res)
    integer :: res(2)
    if ( .not. page_size_set ) call initialize_error_stack_module()
    res = [n_columns, n_rows]
  end function

end module
