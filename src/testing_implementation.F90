submodule(zsfl_testing) zsfl_testing_implementation
  !! category: testing
  !! author: Izaak Beekman
  !!
  !! implementations for [[zsfl_testing(module)]]

contains

  {%- for t in real_types %}

  ! Getters
  module procedure tol_{{t.alias}}_is_absolute
    res = this%tolerance_is_absolute
  end procedure

  ! Type bound procedures & operators
  module procedure within_tol_{{t.alias}}
    select type(tol)
    class is (tolerance_{{t.alias}}_t)
      if (tol%is_absolute()) then
        res = abs(this%lhs - this%rhs) <= tol%tolerance
      else
        res = abs(this%lhs/this%rhs - 1) <= tol%tolerance
      end if
    class default
      ! This is an error condition. Should try to catch these with the error type
    end select
  end procedure

  {%- for ot in real_types %}
  module procedure {{t.alias}}_within_{{ot.alias}}
    res = abs(real(this%lhs,kind={{ot.kind}}) - real(this%rhs,kind={{ot.kind}})) <= tol
  end procedure
  {%- endfor %}

  ! module procedure bare_within_tol_{{t.alias}}
  !   res = lhs%within_tol(rhs)
  ! end procedure

  ! module procedure bare_within_{{t.alias}}
  !   res = lhs%within_{{t.alias}}(rhs)
  ! end procedure

  ! Non-type bound operators
  module procedure absolute_{{t.alias}}
    res%tolerance_is_absolute = .true.
    res%tolerance = tol
  end procedure

  module procedure is_{{t.alias}}
    res%lhs = lhs
    res%rhs = rhs
  end procedure

  module procedure relative_{{t.alias}}
    res%tolerance_is_absolute = .false.
    res%tolerance = tol
  end procedure
  {%- endfor %}

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
