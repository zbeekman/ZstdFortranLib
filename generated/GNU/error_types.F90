module error_list
  !! Module defining error types
  use :: string_helpers, only: LF, get_line
  implicit none
  public

  enum, bind(c)
    enumerator :: status_nominal = 0, &
      memory_error, &
      io_error,  &
      communication_error, &
      divide_by_zero_error, &
      sqrt_of_negative_error, &
      NAN_error, &
      overflow_error, &
      underflow_error, & ! @USER_INJECTED_ERROR_TYPES@
      private_last_error
  end enum

  integer, parameter :: enum_kind = kind(status_nominal)

  character(len=*), parameter :: error_descriptions = &
    "No error, normal operation." // LF // &
    "Memory allocation/deallocation error." // LF // &
    "Input output (IO) error." // LF // &
    "Communication (MPI, Coarrays, OpenMP, etc.) error." // LF // &
    "Divide by zero error." // LF // &
    "Square root of negative number error." // LF // &
    "Not a number error." // LF // &
    "Numeric overflow error." // LF // &
    "Numeric underflow error." // LF // & ! @USER_INJECTED_ERROR_DESCRIPTIONS@
    "Private error type. You should never see this."

  integer, parameter :: end_error_description_pos = scan(error_descriptions, LF, back=.true.)

contains

  pure function get_error_description(error) result(res)
    integer(enum_kind), intent(in) :: error
    character(len=end_error_description_pos) :: res
    integer :: begin, end, pos

    begin = 0
    do pos = 1, error
      begin = scan(error_descriptions(begin:), LF) + 1
    end do
    end = scan(error_descriptions(begin:), LF) - 1
    res = error_descriptions(begin:end)
  end function

end module
