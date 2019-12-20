program testing_implementation_test
  !! category: testing
  !! author: Izaak Beekman
  !!
  !! This test should intentionally fail.
  !! The CMake test property `WILL_FAIL` should be set to `ON`

  use zsfl_testing, only: unit_test_t, &
    {%- for t in real_types %}
    comparison_{{t.alias}}_t, tolerance_{{t.alias}}_t, &
    {%- endfor %}
    operator(.is.), operator(.absolute.), operator(.relative.)

  implicit none

  character(len=*), parameter :: file = &
    _FILE_
  {%- for t in real_types %}
  integer, parameter :: {{t.alias}} = {{t.kind}}
  real(kind={{t.alias}}), parameter :: tolerance_{{t.alias}} = 10.0_{{t.alias}}
  real(kind={{t.alias}}), parameter :: rtolerance_{{t.alias}} = tolerance_{{t.alias}}/100
  real(kind={{t.alias}}), parameter :: one_hundred_{{t.alias}} = 100.0_{{t.alias}}
  {%- endfor %}

  type(unit_test_t) :: test

  call test%initialize(file)

  !> Ensure that overloaded assignment works
  !> The test object has an overloaded assignment that counts errors (assign a .false. condition)
  !> and starts without signaling any errors/failures
  test = .true.
  test = .not. .false.

  !> Test all real kinds
  {%- for t in real_types %}
  !> Test {{t.decl}} interfaces
  {%- for ot in real_types %}
  associate( &
    one_hundred => one_hundred_{{t.alias}}, &
    tolerance   => tolerance_{{t.alias}}, &
    rtolerance  => rtolerance_{{t.alias}})

    ! Trigger failures first
    test = .not. (( (100 - 10*epsilon(1.0_{{t.alias}})) .is. one_hundred ) .within. tolerance_{{ot.alias}})
    test = .not. (( (100 - 10*epsilon(1.0_{{t.alias}})) .is. one_hundred ) .within. .absolute. tolerance)
    test = .not. (( (100 - 10*epsilon(1.0_{{t.alias}})) .is. one_hundred ) .within. .relative. rtolerance)
    test = (111.1_{{t.alias}} .is. one_hundred) .within. tolerance
    test = (111.1_{{t.alias}} .is. one_hundred) .within. .absolute. tolerance
    test = (111.1_{{t.alias}} .is. one_hundred) .within. .relative. rtolerance

    ! Then run successful tests to ensure failures don't get overwritten/reset
    test = ( (100 - 10*epsilon(1.0_{{t.alias}})) .is. one_hundred ) .within. tolerance_{{ot.alias}}
    test = ( (100 - 10*epsilon(1.0_{{t.alias}})) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_{{t.alias}})) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_{{t.alias}} .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_{{t.alias}} .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_{{t.alias}} .is. one_hundred) .within. .relative. rtolerance)
  end associate
  {%- endfor %}
  {%- endfor %}

  write(*,*) "NOTE: This test *SHOULD* fail."
  call test%report_status()
end program
