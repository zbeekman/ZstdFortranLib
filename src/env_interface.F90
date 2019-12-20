module zsfl_env_interface
  !! Interface with the runtime environment
  use zsfl_c_system_interface, only: term_is_a_tty => is_a_tty, get_tty_rows, get_tty_cols
  use zsfl_strings, only: &
    gsub, ascii_k, utf8_k, sub, split, join, to_s, to_i, to_l, to_r, init_float_fmt, nl, &
    colorize => maybe_colorize, use_color, operator(//)
  use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, input_unit
  implicit none

  private
  public :: operator(.env.), operator(.envExists.), OS

  type system_t
    private
    logical :: is_initialized = .false.
    integer :: TTY_LINES = 0, TTY_COLUMNS = 0
    integer :: stdout = output_unit, stderr = error_unit, stdin = input_unit
    logical :: IS_A_TTY = .false.
    character(len=:), allocatable :: DISPLAY, HOME, LANG, LC_ALL, PATH, PWD, SHELL, SHLVL, TERM, TMPDIR, USER
  contains
    procedure, nopass :: env => get_env
    procedure, nopass :: env_exists => in_env
    procedure         :: init => query_environment
    procedure         :: get_info => dump
    procedure, nopass :: tty_connected => term_is_a_tty
!    procedure         :: tty_geom => tty_geometry
!    procedure         :: tty_update_geom => tty_update_geometry
  end type

  interface operator(.envExists.)
    module procedure in_env
  end interface

  interface operator(.env.)
    module procedure get_env
  end interface

  type(system_t) :: OS

contains
  subroutine dump(this)
    class(system_t), intent(inout) :: this

    if ( .not. this%is_initialized) call this%init()
    write(this%stdout, '(A)') '{'
    if ( this%is_a_tty ) then
      write(this%stdout, '(2x,A)') """IS_A_TTY"": true,"
      write(this%stdout, '(*(2x,"""",(A),""": ",(I0),",",:,/))') &
        "COLUMNS", max(this%tty_columns, 0), &
        "LINES", max(this%tty_lines, 0)
    else
      write(this%stdout, '(2x,A)') """IS_A_TTY"": false,"
      if(.envExists. "LINES") write(this%stdout, '(2x,A)') """LINES"": " // this%env("LINES") // ","
      if(.envExists. "COLUMNS") write(this%stdout, '(2x,A)') """COLUMNS"": " // this%env("COLUMNS") // ","
    endif

    write(this%stdout, '(*(2x,"""",(A),""": ","""",(A),"""",:,",",/))') &
      "DISPLAY", this%DISPLAY, &
      "HOME",    this%HOME, &
      "LANG",    this%LANG, &
      "LC_ALL",  this%LC_ALL, &
      "PATH",    this%PATH, &
      "PWD",     this%PWD, &
      "SHELL",   this%SHELL, &
      "SHLVL",   this%SHLVL, &
      "TERM",    this%TERM, &
      "TMPDIR",  this%TMPDIR, &
      "USER",    this%USER

    write(this%stdout, '(A)') '}'
  end subroutine

  subroutine query_environment(this)
    !! Populate protected variables providing information about the computing environment
    class(system_t), intent(inout) :: this

    character(len=:), allocatable :: lines_str, columns_str
    !! Temporary string variables to hold results of environment query

    if (this%is_initialized) return

    ! C functions
    this%IS_A_TTY    = term_is_a_tty()
    this%TTY_LINES   = get_tty_rows()
    this%TTY_COLUMNS = get_tty_cols()

    ! Environment variables that are strings
    this%DISPLAY = this%env("DISPLAY")
    this%HOME    = this%env("HOME")
    this%LANG    = this%env("LANG")
    this%LC_ALL  = this%env("LC_ALL")
    this%PATH    = this%env("PATH")
    this%PWD     = this%env("PWD")
    this%SHELL   = this%env("SHELL")
    this%SHLVL   = this%env("SHLVL")
    this%TERM    = this%env("TERM")
    this%TMPDIR  = this%env("TMPDIR")
    this%USER    = this%env("USER")
  end subroutine

  function get_env(var_name) result(res)
    !! fetch environment variable
    character(len=*), intent(in) :: var_name
    character(len=:), allocatable :: res
    integer :: var_len, var_status

    call get_environment_variable(var_name, length=var_len, status=var_status)
    select case (var_status)
    case(1)
      allocate(res, source=var_name//"-NOTFOUND")
    case(2)
      allocate(res, source="ERROR: Environment variables not supported.")
    case(3:)
      allocate(res, source="ERROR: Unspecified error retrieving environment variable.")
    case(0)
      allocate(character(len=var_len)::res)
      call get_environment_variable(var_name, value=res, status=var_status)
    end select
  end function

  function in_env(var_name) result(res)
    !! Check if variable is defined in the environment
    character(len=*), intent(in) :: var_name
    logical :: res
    integer :: var_status

    call get_environment_variable(var_name, status=var_status)
    res = (var_status == 0)
  end function

end module
