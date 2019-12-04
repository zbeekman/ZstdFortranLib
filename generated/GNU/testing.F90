module zsfl_testing
  !! category: testing
  !! author: Izaak Beekman
  !!

  use zsfl_testing_interface, only: &
    comparison_t, tolerance_t

  implicit none

  private
  public :: &
    operator(.absolute.), operator(.is.), operator(.relative.), & !operator(.within.), &
    unit_test_t, &
    comparison_r4_t, &
    tolerance_r4_t, &
    comparison_r8_t, &
    tolerance_r8_t, &
    comparison_r10_t, &
    tolerance_r10_t, &
    comparison_r16_t, &
    tolerance_r16_t
  ! real(4) types & interfaces

  !> real(4) extension of `[[comparison_t]]`
  type, extends(comparison_t) :: comparison_r4_t
    private
    real(4) :: lhs
    real(4) :: rhs
  contains
    procedure :: within_tol => within_tol_r4
    procedure :: within_r4 => r4_within_r4
    procedure :: within_r8 => r4_within_r8
    procedure :: within_r10 => r4_within_r10
    procedure :: within_r16 => r4_within_r16
!    generic :: operator(.within.) => within_r4
  end type

  !> real(4) extension of `[[tolerance_t]]`
  type, extends(tolerance_t) :: tolerance_r4_t
    private
    logical :: tolerance_is_absolute = .true.
    real(4) :: tolerance
  contains
    procedure :: is_absolute => tol_r4_is_absolute
  end type

  interface
    pure module function tol_r4_is_absolute(this) result(res)
      class(tolerance_r4_t), intent(in) :: this
      logical res
    end function
    pure module function within_tol_r4(this,tol) result(res)
      !! Deferred implementation of `within_tol` from `[[comparison_t]]`
      class(comparison_r4_t), intent(in) :: this
      class(tolerance_t), intent(in) :: tol
      logical :: res
    end function
    pure module function r4_within_r4(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r4_t), intent(in) :: this
      real(4), intent(in) :: tol
      logical :: res
    end function
    pure module function r4_within_r8(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r4_t), intent(in) :: this
      real(8), intent(in) :: tol
      logical :: res
    end function
    pure module function r4_within_r10(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r4_t), intent(in) :: this
      real(10), intent(in) :: tol
      logical :: res
    end function
    pure module function r4_within_r16(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r4_t), intent(in) :: this
      real(16), intent(in) :: tol
      logical :: res
    end function
  end interface

  ! interface operator(.within.)
  !   pure module function bare_within_tol_r4(lhs,rhs) result(res)
  !     type(comparison_r4_t), intent(in) :: lhs
  !     type(tolerance_r4_t), intent(in) :: rhs
  !     logical :: res
  !   end function
  !   pure module function bare_within_r4(lhs,rhs) result(res)
  !     type(comparison_r4_t), intent(in) :: lhs
  !     real(4), intent(in) :: rhs
  !     logical :: res
  !   end function
  ! end interface
  ! real(8) types & interfaces

  !> real(8) extension of `[[comparison_t]]`
  type, extends(comparison_t) :: comparison_r8_t
    private
    real(8) :: lhs
    real(8) :: rhs
  contains
    procedure :: within_tol => within_tol_r8
    procedure :: within_r4 => r8_within_r4
    procedure :: within_r8 => r8_within_r8
    procedure :: within_r10 => r8_within_r10
    procedure :: within_r16 => r8_within_r16
!    generic :: operator(.within.) => within_r8
  end type

  !> real(8) extension of `[[tolerance_t]]`
  type, extends(tolerance_t) :: tolerance_r8_t
    private
    logical :: tolerance_is_absolute = .true.
    real(8) :: tolerance
  contains
    procedure :: is_absolute => tol_r8_is_absolute
  end type

  interface
    pure module function tol_r8_is_absolute(this) result(res)
      class(tolerance_r8_t), intent(in) :: this
      logical res
    end function
    pure module function within_tol_r8(this,tol) result(res)
      !! Deferred implementation of `within_tol` from `[[comparison_t]]`
      class(comparison_r8_t), intent(in) :: this
      class(tolerance_t), intent(in) :: tol
      logical :: res
    end function
    pure module function r8_within_r4(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r8_t), intent(in) :: this
      real(4), intent(in) :: tol
      logical :: res
    end function
    pure module function r8_within_r8(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r8_t), intent(in) :: this
      real(8), intent(in) :: tol
      logical :: res
    end function
    pure module function r8_within_r10(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r8_t), intent(in) :: this
      real(10), intent(in) :: tol
      logical :: res
    end function
    pure module function r8_within_r16(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r8_t), intent(in) :: this
      real(16), intent(in) :: tol
      logical :: res
    end function
  end interface

  ! interface operator(.within.)
  !   pure module function bare_within_tol_r8(lhs,rhs) result(res)
  !     type(comparison_r8_t), intent(in) :: lhs
  !     type(tolerance_r8_t), intent(in) :: rhs
  !     logical :: res
  !   end function
  !   pure module function bare_within_r8(lhs,rhs) result(res)
  !     type(comparison_r8_t), intent(in) :: lhs
  !     real(8), intent(in) :: rhs
  !     logical :: res
  !   end function
  ! end interface
  ! real(10) types & interfaces

  !> real(10) extension of `[[comparison_t]]`
  type, extends(comparison_t) :: comparison_r10_t
    private
    real(10) :: lhs
    real(10) :: rhs
  contains
    procedure :: within_tol => within_tol_r10
    procedure :: within_r4 => r10_within_r4
    procedure :: within_r8 => r10_within_r8
    procedure :: within_r10 => r10_within_r10
    procedure :: within_r16 => r10_within_r16
!    generic :: operator(.within.) => within_r10
  end type

  !> real(10) extension of `[[tolerance_t]]`
  type, extends(tolerance_t) :: tolerance_r10_t
    private
    logical :: tolerance_is_absolute = .true.
    real(10) :: tolerance
  contains
    procedure :: is_absolute => tol_r10_is_absolute
  end type

  interface
    pure module function tol_r10_is_absolute(this) result(res)
      class(tolerance_r10_t), intent(in) :: this
      logical res
    end function
    pure module function within_tol_r10(this,tol) result(res)
      !! Deferred implementation of `within_tol` from `[[comparison_t]]`
      class(comparison_r10_t), intent(in) :: this
      class(tolerance_t), intent(in) :: tol
      logical :: res
    end function
    pure module function r10_within_r4(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r10_t), intent(in) :: this
      real(4), intent(in) :: tol
      logical :: res
    end function
    pure module function r10_within_r8(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r10_t), intent(in) :: this
      real(8), intent(in) :: tol
      logical :: res
    end function
    pure module function r10_within_r10(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r10_t), intent(in) :: this
      real(10), intent(in) :: tol
      logical :: res
    end function
    pure module function r10_within_r16(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r10_t), intent(in) :: this
      real(16), intent(in) :: tol
      logical :: res
    end function
  end interface

  ! interface operator(.within.)
  !   pure module function bare_within_tol_r10(lhs,rhs) result(res)
  !     type(comparison_r10_t), intent(in) :: lhs
  !     type(tolerance_r10_t), intent(in) :: rhs
  !     logical :: res
  !   end function
  !   pure module function bare_within_r10(lhs,rhs) result(res)
  !     type(comparison_r10_t), intent(in) :: lhs
  !     real(10), intent(in) :: rhs
  !     logical :: res
  !   end function
  ! end interface
  ! real(16) types & interfaces

  !> real(16) extension of `[[comparison_t]]`
  type, extends(comparison_t) :: comparison_r16_t
    private
    real(16) :: lhs
    real(16) :: rhs
  contains
    procedure :: within_tol => within_tol_r16
    procedure :: within_r4 => r16_within_r4
    procedure :: within_r8 => r16_within_r8
    procedure :: within_r10 => r16_within_r10
    procedure :: within_r16 => r16_within_r16
!    generic :: operator(.within.) => within_r16
  end type

  !> real(16) extension of `[[tolerance_t]]`
  type, extends(tolerance_t) :: tolerance_r16_t
    private
    logical :: tolerance_is_absolute = .true.
    real(16) :: tolerance
  contains
    procedure :: is_absolute => tol_r16_is_absolute
  end type

  interface
    pure module function tol_r16_is_absolute(this) result(res)
      class(tolerance_r16_t), intent(in) :: this
      logical res
    end function
    pure module function within_tol_r16(this,tol) result(res)
      !! Deferred implementation of `within_tol` from `[[comparison_t]]`
      class(comparison_r16_t), intent(in) :: this
      class(tolerance_t), intent(in) :: tol
      logical :: res
    end function
    pure module function r16_within_r4(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r16_t), intent(in) :: this
      real(4), intent(in) :: tol
      logical :: res
    end function
    pure module function r16_within_r8(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r16_t), intent(in) :: this
      real(8), intent(in) :: tol
      logical :: res
    end function
    pure module function r16_within_r10(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r16_t), intent(in) :: this
      real(10), intent(in) :: tol
      logical :: res
    end function
    pure module function r16_within_r16(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_r16_t), intent(in) :: this
      real(16), intent(in) :: tol
      logical :: res
    end function
  end interface

  ! interface operator(.within.)
  !   pure module function bare_within_tol_r16(lhs,rhs) result(res)
  !     type(comparison_r16_t), intent(in) :: lhs
  !     type(tolerance_r16_t), intent(in) :: rhs
  !     logical :: res
  !   end function
  !   pure module function bare_within_r16(lhs,rhs) result(res)
  !     type(comparison_r16_t), intent(in) :: lhs
  !     real(16), intent(in) :: rhs
  !     logical :: res
  !   end function
  ! end interface

  interface operator(.absolute.)
    !! return an absolute `[[tolerance_t]]` object from a real
    pure module function absolute_r4(tol) result(res)
      real(4), intent(in) :: tol
      type(tolerance_r4_t) :: res
    end function
    pure module function absolute_r8(tol) result(res)
      real(8), intent(in) :: tol
      type(tolerance_r8_t) :: res
    end function
    pure module function absolute_r10(tol) result(res)
      real(10), intent(in) :: tol
      type(tolerance_r10_t) :: res
    end function
    pure module function absolute_r16(tol) result(res)
      real(16), intent(in) :: tol
      type(tolerance_r16_t) :: res
    end function
  end interface

  interface operator(.is.)
    !! return a `[[comparison_t]]` object from two reals
    pure module function is_r4(lhs, rhs) result(res)
      real(4), intent(in) :: lhs, rhs
      type(comparison_r4_t) :: res
    end function
    pure module function is_r8(lhs, rhs) result(res)
      real(8), intent(in) :: lhs, rhs
      type(comparison_r8_t) :: res
    end function
    pure module function is_r10(lhs, rhs) result(res)
      real(10), intent(in) :: lhs, rhs
      type(comparison_r10_t) :: res
    end function
    pure module function is_r16(lhs, rhs) result(res)
      real(16), intent(in) :: lhs, rhs
      type(comparison_r16_t) :: res
    end function
  end interface

  interface operator(.relative.)
    !! return a relative `[[tolerance_t]]` object from a real
    pure module function relative_r4(tol) result(res)
      real(4), intent(in) :: tol
      type(tolerance_r4_t) :: res
    end function
    pure module function relative_r8(tol) result(res)
      real(8), intent(in) :: tol
      type(tolerance_r8_t) :: res
    end function
    pure module function relative_r10(tol) result(res)
      real(10), intent(in) :: tol
      type(tolerance_r10_t) :: res
    end function
    pure module function relative_r16(tol) result(res)
      real(16), intent(in) :: tol
      type(tolerance_r16_t) :: res
    end function
  end interface operator(.relative.)

  type :: unit_test_t
    private
    integer :: n_subtests = 0
    integer :: n_failures = 0
    logical :: signaling = .false.
    character(len=:), allocatable :: file_name
  contains
    procedure :: initialize => unit_test_init
    procedure :: report_status => test_report
    procedure :: increment => increment_failures
    generic :: assignment(=) => increment
  end type

  interface
    pure module subroutine unit_test_init(this,filename)
      class(unit_test_t), intent(inout) :: this
      character(len=*), intent(in) :: filename
    end subroutine
    pure module subroutine increment_failures(this,passed)
      class(unit_test_t), intent(inout) :: this
      logical, intent(in) :: passed
    end subroutine
    module subroutine test_report(this)
      class(unit_test_t), intent(in) :: this
    end subroutine
  end interface

end module
