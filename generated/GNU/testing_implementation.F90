submodule(zsfl_testing) zsfl_testing_implementation
  !! category: testing
  !! author: Izaak Beekman
  !!
  !! implementations for [[zsfl_testing(module)]]

contains

  ! Getters
  module procedure tol_r4_is_absolute
    res = this%tolerance_is_absolute
  end procedure

  ! Type bound procedures & operators
  module procedure within_tol_r4
    select type(tol)
    class is (tolerance_r4_t)
      if (tol%is_absolute()) then
        res = abs(this%lhs - this%rhs) <= tol%tolerance
      else
        res = abs(this%lhs/this%rhs - 1) <= tol%tolerance
      end if
    class default
      ! This is an error condition. Should try to catch these with the error type
    end select
  end procedure
  module procedure r4_within_r4
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r4_within_r8
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r4_within_r10
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r4_within_r16
    res = abs(this%lhs - this%rhs) <= tol
  end procedure

  ! module procedure bare_within_tol_r4
  !   res = lhs%within_tol(rhs)
  ! end procedure

  ! module procedure bare_within_r4
  !   res = lhs%within_r4(rhs)
  ! end procedure

  ! Non-type bound operators
  module procedure absolute_r4
    res%tolerance_is_absolute = .true.
    res%tolerance = tol
  end procedure

  module procedure is_r4
    res%lhs = lhs
    res%rhs = rhs
  end procedure

  module procedure relative_r4
    res%tolerance_is_absolute = .false.
    res%tolerance = tol
  end procedure

  ! Getters
  module procedure tol_r8_is_absolute
    res = this%tolerance_is_absolute
  end procedure

  ! Type bound procedures & operators
  module procedure within_tol_r8
    select type(tol)
    class is (tolerance_r8_t)
      if (tol%is_absolute()) then
        res = abs(this%lhs - this%rhs) <= tol%tolerance
      else
        res = abs(this%lhs/this%rhs - 1) <= tol%tolerance
      end if
    class default
      ! This is an error condition. Should try to catch these with the error type
    end select
  end procedure
  module procedure r8_within_r4
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r8_within_r8
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r8_within_r10
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r8_within_r16
    res = abs(this%lhs - this%rhs) <= tol
  end procedure

  ! module procedure bare_within_tol_r8
  !   res = lhs%within_tol(rhs)
  ! end procedure

  ! module procedure bare_within_r8
  !   res = lhs%within_r8(rhs)
  ! end procedure

  ! Non-type bound operators
  module procedure absolute_r8
    res%tolerance_is_absolute = .true.
    res%tolerance = tol
  end procedure

  module procedure is_r8
    res%lhs = lhs
    res%rhs = rhs
  end procedure

  module procedure relative_r8
    res%tolerance_is_absolute = .false.
    res%tolerance = tol
  end procedure

  ! Getters
  module procedure tol_r10_is_absolute
    res = this%tolerance_is_absolute
  end procedure

  ! Type bound procedures & operators
  module procedure within_tol_r10
    select type(tol)
    class is (tolerance_r10_t)
      if (tol%is_absolute()) then
        res = abs(this%lhs - this%rhs) <= tol%tolerance
      else
        res = abs(this%lhs/this%rhs - 1) <= tol%tolerance
      end if
    class default
      ! This is an error condition. Should try to catch these with the error type
    end select
  end procedure
  module procedure r10_within_r4
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r10_within_r8
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r10_within_r10
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r10_within_r16
    res = abs(this%lhs - this%rhs) <= tol
  end procedure

  ! module procedure bare_within_tol_r10
  !   res = lhs%within_tol(rhs)
  ! end procedure

  ! module procedure bare_within_r10
  !   res = lhs%within_r10(rhs)
  ! end procedure

  ! Non-type bound operators
  module procedure absolute_r10
    res%tolerance_is_absolute = .true.
    res%tolerance = tol
  end procedure

  module procedure is_r10
    res%lhs = lhs
    res%rhs = rhs
  end procedure

  module procedure relative_r10
    res%tolerance_is_absolute = .false.
    res%tolerance = tol
  end procedure

  ! Getters
  module procedure tol_r16_is_absolute
    res = this%tolerance_is_absolute
  end procedure

  ! Type bound procedures & operators
  module procedure within_tol_r16
    select type(tol)
    class is (tolerance_r16_t)
      if (tol%is_absolute()) then
        res = abs(this%lhs - this%rhs) <= tol%tolerance
      else
        res = abs(this%lhs/this%rhs - 1) <= tol%tolerance
      end if
    class default
      ! This is an error condition. Should try to catch these with the error type
    end select
  end procedure
  module procedure r16_within_r4
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r16_within_r8
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r16_within_r10
    res = abs(this%lhs - this%rhs) <= tol
  end procedure
  module procedure r16_within_r16
    res = abs(this%lhs - this%rhs) <= tol
  end procedure

  ! module procedure bare_within_tol_r16
  !   res = lhs%within_tol(rhs)
  ! end procedure

  ! module procedure bare_within_r16
  !   res = lhs%within_r16(rhs)
  ! end procedure

  ! Non-type bound operators
  module procedure absolute_r16
    res%tolerance_is_absolute = .true.
    res%tolerance = tol
  end procedure

  module procedure is_r16
    res%lhs = lhs
    res%rhs = rhs
  end procedure

  module procedure relative_r16
    res%tolerance_is_absolute = .false.
    res%tolerance = tol
  end procedure

  ! Unit test TBPs
  module procedure unit_test_init
    allocate(this%file_name, source=trim(filename))
  end procedure

  module procedure increment_failures
    this%n_subtests = this%n_subtests + 1
    this%signaling = this%signaling .or. .not. passed
    if( .not. passed ) this%n_failures = this%n_failures + 1
  end procedure

  module procedure test_report
    use zsfl_strings, only: &
      nl, &
      operator(//), &
      bold, green, red, underline, yellow
    if(this%n_failures == 0) then
      write(*,"(A)") ""
      write(*,"(A)") "All " // this%n_subtests // " sub-test(s) passed in:"
      write(*,"(A)") "    " // this%file_name
      write(*,"(A)") ""
      write(*,"(A)") bold(green("Test passed."))
      write(*,"(A)") ""
    else
      write(*,"(A)") ""
      write(*,"(A)") "Detected " // bold(red(this%n_failures // "failure(s)")) // " of " // this%n_subtests // " sub-test(s):"
      write(*,"(A)") bold("    " // this%file_name)
      write(*,"(A)") ""
      write(*,"(A)") underline(red("Test failed!"))
      write(*,"(A)") ""
    end if
  end procedure

end submodule
