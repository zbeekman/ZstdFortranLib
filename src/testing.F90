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
    {%- for t in real_types %}
    comparison_{{t.alias}}_t, &
    tolerance_{{t.alias}}_t
    {%- if not loop.last -%} , & {%- endif %}
    {%- endfor %}

  {%- for t in real_types %}
  ! {{t.decl}} types & interfaces

  !> {{t.decl}} extension of `[[comparison_t]]`
  type, extends(comparison_t) :: comparison_{{t.alias}}_t
    private
    {{t.decl}} :: lhs
    {{t.decl}} :: rhs
  contains
    procedure :: within_tol => within_tol_{{t.alias}}
    {%- for ot in real_types %}
    procedure :: within_{{ot.alias}} => {{t.alias}}_within_{{ot.alias}}
    {%- endfor %}
!    generic :: operator(.within.) => within_{{t.alias}}
  end type

  !> {{t.decl}} extension of `[[tolerance_t]]`
  type, extends(tolerance_t) :: tolerance_{{t.alias}}_t
    private
    logical :: tolerance_is_absolute = .true.
    {{t.decl}} :: tolerance
  contains
    procedure :: is_absolute => tol_{{t.alias}}_is_absolute
  end type

  interface
    pure module function tol_{{t.alias}}_is_absolute(this) result(res)
      class(tolerance_{{t.alias}}_t), intent(in) :: this
      logical res
    end function
    pure module function within_tol_{{t.alias}}(this,tol) result(res)
      !! Deferred implementation of `within_tol` from `[[comparison_t]]`
      class(comparison_{{t.alias}}_t), intent(in) :: this
      class(tolerance_t), intent(in) :: tol
      logical :: res
    end function
    {%- for ot in real_types %}
    pure module function {{t.alias}}_within_{{ot.alias}}(this,tol) result(res)
      !! Deferred implementation when the `.absolute.` and `.relative.` operators are not used
      class(comparison_{{t.alias}}_t), intent(in) :: this
      {{ot.decl}}, intent(in) :: tol
      logical :: res
    end function
    {%- endfor %}
  end interface

  ! interface operator(.within.)
  !   pure module function bare_within_tol_{{t.alias}}(lhs,rhs) result(res)
  !     type(comparison_{{t.alias}}_t), intent(in) :: lhs
  !     type(tolerance_{{t.alias}}_t), intent(in) :: rhs
  !     logical :: res
  !   end function
  !   pure module function bare_within_{{t.alias}}(lhs,rhs) result(res)
  !     type(comparison_{{t.alias}}_t), intent(in) :: lhs
  !     {{t.decl}}, intent(in) :: rhs
  !     logical :: res
  !   end function
  ! end interface

  {%- endfor %}

  interface operator(.absolute.)
    !! return an absolute `[[tolerance_t]]` object from a real
    {%- for t in real_types %}
    pure module function absolute_{{t.alias}}(tol) result(res)
      {{t.decl}}, intent(in) :: tol
      type(tolerance_{{t.alias}}_t) :: res
    end function
    {%- endfor %}
  end interface

  interface operator(.is.)
    !! return a `[[comparison_t]]` object from two reals
    {%- for t in real_types %}
    pure module function is_{{t.alias}}(lhs, rhs) result(res)
      {{t.decl}}, intent(in) :: lhs, rhs
      type(comparison_{{t.alias}}_t) :: res
    end function
    {%- endfor %}
  end interface

  interface operator(.relative.)
    !! return a relative `[[tolerance_t]]` object from a real
    {%- for t in real_types %}
    pure module function relative_{{t.alias}}(tol) result(res)
      {{t.decl}}, intent(in) :: tol
      type(tolerance_{{t.alias}}_t) :: res
    end function
    {%- endfor %}
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
