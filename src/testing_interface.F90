module zsfl_testing_interface
  !! category: testing
  !! author: Izaak Beekman
  !!
  !! Module to provide types, classes and functions for testing and comparisons
  !!
  !! ## Syntactic sugar
  !! Classes and operators are provided to write tests in a manner that mimics natural language.
  !! In particular, tests can be composed using a natural syntax where one writes:
  !!
  !!     measurement .is. expected_result .within. tolerance
  !!
  !! and `measurement`, `expected_result` and `tolerance` are all real variables, named constants or literals.
  !! The result of the expression above is a logical value of the default kind which is `.true.` if and only if
  !! \[ abs(measurement - expected\_result) \leq tolerance \]
  !!
  !! Alternate formulations are allowed as well, using the `.absolute.` (default behavior) and `.relative.` operators.
  !! The `.relative.` comparison operator changes the comparison to look at the relative error, and interprets the tolerance
  !! as a relative tolerance. For example, the expression
  !!
  !!     measurement .is. expected_result .within. .relative. tolerance
  !!
  !! will evaluate to `.true.` if and only if
  !! \[ abs(\frac{measurement - expected}{expected}) \leq tolerance \text{.}\]
  !!
  !! The `.absolute.` operator is the default behavior and is not required.
  !!
  !! ## Abstract tolerance and comparison classes
  !! The abstract `comparison_t` and `tolerance_t` classes must have concrete implementations that
  !! store various real kinds and can be used in composition with the named operators to achieve the desired effect.
  !! While one could use parameterized derived types to achieve compatibility with numerous real kinds,
  !! compiler support for PDTs is notoriously buggy, and it is very challenging to generate efficient, correct code.
  !! Until support improves, the concrete implementations of these abstract classes, and corresponding operators
  !! are responsible for providing support for all available real kinds.
  !!
  !! ## Abstract unit test class
  !! `unit_test_t` provides an abstract class to setup and monitor tests. It provides capabilities to perform
  !! multiple sub-tests, and then report the results at the end of execution.

  implicit none

  private
  public :: comparison_t, tolerance_t, &
    operator(.absolute.), operator(.is.), operator(.relative.)

  !> category: testing
  !>
  !> abstract comparison type returned by `.is.` operator for floating point
  !> comparison with tolerances
  type, abstract :: comparison_t
    private
  contains
    procedure(logical_fn_comparison_tolerance), deferred :: within_tol
    {%- for t in real_types %}
    procedure(logical_fn_comparison_{{t.alias}}), deferred :: within_{{t.alias}}
    {%- endfor %}
    !! within tolerance (absolute or relative) specified by tolerance object
    generic :: operator(.within.) => &
      {%- for t in real_types %}
      within_{{t.alias}}, &
      {%- endfor %}
      within_tol
    !! Syntactic sugar: `.within.` operator
  end type

  !> category: testing
  !>
  !> abstract tolerance type returned by `.relative.` and `.absolute.` operators
  !>
  !> This objects from this class are used with the `.within.` operator of the `[[comparison_t]]`
  !> to return `.true.` or `.false.` if the comparison is within the specified tolerance.
  type, abstract :: tolerance_t
  contains
    procedure(logical_fn_tolerance), deferred :: is_absolute
  end type

  abstract interface
    pure module function logical_fn_tolerance(this) result(res)
      class(tolerance_t), intent(in) :: this
      logical :: res
    end function
    pure module function logical_fn_comparison_tolerance(this,tol) result(res)
      class(comparison_t), intent(in) :: this
      class(tolerance_t), intent(in) :: tol
      logical :: res
    end function
    {%- for t in real_types %}
    pure module function logical_fn_comparison_{{t.alias}}(this,tol) result(res)
      class(comparison_t), intent(in) :: this
      {{t.decl}}, intent(in) :: tol
      logical :: res
    end function
    {%- endfor %}

  end interface

end module
