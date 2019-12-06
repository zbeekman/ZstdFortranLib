program testing_test
  !! category: testing
  !! author: Izaak Beekman
  !!

  use zsfl_testing, only: unit_test_t, &
    comparison_r4_t, tolerance_r4_t, &
    comparison_r8_t, tolerance_r8_t, &
    comparison_r10_t, tolerance_r10_t, &
    comparison_r16_t, tolerance_r16_t, &
    operator(.is.), operator(.absolute.), operator(.relative.)

  implicit none

  character(len=*), parameter :: file = &
    __FILE__
  integer, parameter :: r4 = 4
  real(kind=r4), parameter :: tolerance_r4 = 10.0_r4
  real(kind=r4), parameter :: rtolerance_r4 = tolerance_r4/100
  real(kind=r4), parameter :: one_hundred_r4 = 100.0_r4
  integer, parameter :: r8 = 8
  real(kind=r8), parameter :: tolerance_r8 = 10.0_r8
  real(kind=r8), parameter :: rtolerance_r8 = tolerance_r8/100
  real(kind=r8), parameter :: one_hundred_r8 = 100.0_r8
  integer, parameter :: r10 = 10
  real(kind=r10), parameter :: tolerance_r10 = 10.0_r10
  real(kind=r10), parameter :: rtolerance_r10 = tolerance_r10/100
  real(kind=r10), parameter :: one_hundred_r10 = 100.0_r10
  integer, parameter :: r16 = 16
  real(kind=r16), parameter :: tolerance_r16 = 10.0_r16
  real(kind=r16), parameter :: rtolerance_r16 = tolerance_r16/100
  real(kind=r16), parameter :: one_hundred_r16 = 100.0_r16

  type(unit_test_t) :: test

  call test%initialize(file)

  !> Ensure that overloaded assignment works
  !> The test object has an overloaded assignment that counts errors (assign a .false. condition)
  !> and starts without signaling any errors/failures
  test = .true.
  test = .not. .false.

  !> Test all real kinds
  !> Test real(4) interfaces
  associate( &
    one_hundred => one_hundred_r4, &
    tolerance   => tolerance_r4, &
    rtolerance  => rtolerance_r4)

    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. tolerance_r4
    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r4 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r4 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r4 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r4, &
    tolerance   => tolerance_r4, &
    rtolerance  => rtolerance_r4)

    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. tolerance_r8
    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r4 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r4 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r4 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r4, &
    tolerance   => tolerance_r4, &
    rtolerance  => rtolerance_r4)

    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. tolerance_r10
    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r4 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r4 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r4 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r4, &
    tolerance   => tolerance_r4, &
    rtolerance  => rtolerance_r4)

    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. tolerance_r16
    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r4)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r4 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r4 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r4 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  !> Test real(8) interfaces
  associate( &
    one_hundred => one_hundred_r8, &
    tolerance   => tolerance_r8, &
    rtolerance  => rtolerance_r8)

    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. tolerance_r4
    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r8 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r8 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r8 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r8, &
    tolerance   => tolerance_r8, &
    rtolerance  => rtolerance_r8)

    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. tolerance_r8
    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r8 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r8 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r8 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r8, &
    tolerance   => tolerance_r8, &
    rtolerance  => rtolerance_r8)

    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. tolerance_r10
    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r8 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r8 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r8 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r8, &
    tolerance   => tolerance_r8, &
    rtolerance  => rtolerance_r8)

    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. tolerance_r16
    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r8)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r8 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r8 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r8 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  !> Test real(10) interfaces
  associate( &
    one_hundred => one_hundred_r10, &
    tolerance   => tolerance_r10, &
    rtolerance  => rtolerance_r10)

    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. tolerance_r4
    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r10 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r10 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r10 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r10, &
    tolerance   => tolerance_r10, &
    rtolerance  => rtolerance_r10)

    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. tolerance_r8
    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r10 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r10 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r10 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r10, &
    tolerance   => tolerance_r10, &
    rtolerance  => rtolerance_r10)

    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. tolerance_r10
    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r10 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r10 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r10 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r10, &
    tolerance   => tolerance_r10, &
    rtolerance  => rtolerance_r10)

    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. tolerance_r16
    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r10)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r10 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r10 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r10 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  !> Test real(16) interfaces
  associate( &
    one_hundred => one_hundred_r16, &
    tolerance   => tolerance_r16, &
    rtolerance  => rtolerance_r16)

    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. tolerance_r4
    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r16 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r16 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r16 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r16, &
    tolerance   => tolerance_r16, &
    rtolerance  => rtolerance_r16)

    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. tolerance_r8
    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r16 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r16 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r16 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r16, &
    tolerance   => tolerance_r16, &
    rtolerance  => rtolerance_r16)

    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. tolerance_r10
    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r16 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r16 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r16 .is. one_hundred) .within. .relative. rtolerance)
  end associate
  associate( &
    one_hundred => one_hundred_r16, &
    tolerance   => tolerance_r16, &
    rtolerance  => rtolerance_r16)

    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. tolerance_r16
    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. .absolute. tolerance
    test = ( (100 - 10*epsilon(1.0_r16)) .is. one_hundred ) .within. .relative. rtolerance
    test = .not. ((10.0_r16 .is. one_hundred) .within. tolerance)
    test = .not. ((10.0_r16 .is. one_hundred) .within. .absolute. tolerance)
    test = .not. ((10.0_r16 .is. one_hundred) .within. .relative. rtolerance)
  end associate

  call test%report_status()
end program
