program test_string_mod
  use zsfl_strings, only : &
    gsub, ascii_k, utf8_k, sub, split, join, to_s, to_i, to_l, to_r, init_float_fmt, nl, &
    colorize => maybe_colorize, use_color, underline, bold, inverse, strikethrough, &
    green, red, yellow, operator(//)
  use, intrinsic :: iso_fortran_env, only: stderr => error_unit, stdout => output_unit
  implicit none

  ! Give us a fighting chance of getting the filename into 132 characters
  character(len=*), parameter :: THIS_FILE = &
    _FILE_
{% for t in character_types %}
  character(kind={{t.kind}},len=*), parameter :: &
    qbf_{{t.alias}} = {{t.kind}}_"the quick brown fox jumped over the lazy dog"
  character(kind={{t.kind}},len=*), parameter :: &
    paths_{{t.alias}} = {{t.kind}}_"/bin:/sbin:/usr/bin:/usr/local/bin"
  character(kind={{t.kind}},len=:), allocatable :: output_{{t.alias}}
  character(kind={{t.kind}},len=:), allocatable :: page_{{t.alias}}(:)
{% endfor %}
  integer :: i, failures = 0, n_tests = 0
  character(len=:), allocatable :: failed_lines

{% for generic, kinds in [("string_expect", character_types), ("assert_delayed", logical_types)] %}
  interface {{generic}}
    {%- for t in kinds %}
    procedure {{generic}}_{{t.alias}}
    {%- endfor %}
  end interface
{% endfor %}

call init_float_fmt(6)
call use_color(.true.)

!  call use_color(.false.)
  write(stdout,"(A)") ""
  write(stdout,"(A,I0)") "Default kind = ", selected_char_kind("default")
  write(stdout,"(A,I0)") "Ascii kind   = ", ascii_k
  write(stdout,"(A,I0)") "Unicode kind = ", utf8_k
  ! call init_float_fmt()

{% for t in character_types %}
#define PAGE_ page_{{t.alias}}
#define _CK_ {{t.kind}}_
#define OUTPUT_ output_{{t.alias}}

  write(stdout,"(A)") yellow(_CK_"Testing " //underline(_CK_"{{t.alias}}"))
  write(stdout,"(A)") bold(_CK_"This text is "//green(_CK_"green"))
  write(stdout,"(A)") red(_CK_"This text is "//strikethrough(_CK_"not")//_CK_" red")
  write(stdout,"(A)") inverse(_CK_"Inverse text.")

  associate(paths => paths_{{t.alias}}, qbf => qbf_{{t.alias}})
    call use_color(.false.)
    OUTPUT_ = underline( yellow(qbf))
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "underline( yellow( {{t.decl}} )) (color = OFF) failed")
    OUTPUT_ = bold( green(qbf))
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "bold( green( {{t.decl}} )) (color = OFF) failed")
    OUTPUT_ = red( strikethrough(qbf))
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "red( strikethrough( {{t.decl}} )) (color = OFF) failed")
    OUTPUT_ = inverse(qbf)
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "inverse( {{t.decl}} ) (color = OFF) failed")
    call use_color(.true.)

    OUTPUT_ = gsub(qbf, _CK_"the", _CK_"a")
    call assert_delayed ( &
      OUTPUT_ ==  _CK_"a quick brown fox jumped over a lazy dog", __LINE__ , &
      "{{t.decl}} gsub 'the --> a' substitution failed")

    call assert_delayed ( gsub(qbf, _CK_"moose", _CK_"fox") == qbf, __LINE__ , &
      "{{t.decl}} gsub no-op failed")

    OUTPUT_ = gsub(qbf, _CK_"dog", _CK_"god")
    call assert_delayed ( &
      OUTPUT_ == _CK_"the quick brown fox jumped over the lazy god", __LINE__ , &
      "{{t.decl}} gsub 'dog --> god' failed")

    OUTPUT_ = sub(qbf, _CK_"dog", _CK_"god")
    call assert_delayed ( &
      OUTPUT_ == _CK_"the quick brown fox jumped over the lazy god", __LINE__ , &
      "{{t.decl}} sub 'dog --> god' failed")

    OUTPUT_ = sub(qbf, _CK_"the", _CK_"a")
    call assert_delayed ( &
      OUTPUT_ == _CK_"a quick brown fox jumped over the lazy dog", __LINE__ , &
      "{{t.decl}} sub (left) 'the --> a' failed")

    OUTPUT_ = sub(qbf, _CK_"the", _CK_"a", back=.true.)
    call assert_delayed ( &
      OUTPUT_ == _CK_"the quick brown fox jumped over a lazy dog", __LINE__ , &
      "{{t.decl}} sub (right) 'the --> a' failed")

    OUTPUT_ = sub(qbf, _CK_"chicken", _CK_"rooster")
    call assert_delayed ( &
      OUTPUT_ == qbf, __LINE__ , &
      "{{t.decl}} sub no-op failed")

    allocate( PAGE_ , source = split(paths, _CK_":"))
    associate(msg => "{{t.decl}} path splitting test failed")
      {%- for str in ["/bin", "/sbin", "/usr/bin", "/usr/local/bin"] %}
      call assert_delayed ( PAGE_ ({{loop.index}}) == _CK_"{{str}}", __LINE__, msg)
      {%- endfor %}
    end associate

    deallocate( PAGE_ )
    allocate( PAGE_ , source = split(paths, _CK_""))
    do i = 1, size( PAGE_ )
      call assert_delayed ( PAGE_ (i) == paths, __LINE__ , &
        "{{t.decl}} no-op split on empty string test failed")
    end do

    deallocate( PAGE_ )
    allocate( PAGE_ , source = split(paths, _CK_";"))
    do i = 1, size( PAGE_ )
      call assert_delayed ( PAGE_ (i) == paths, __LINE__ , &
        "{{t.decl}} no-op split test failed")
    end do
    deallocate( PAGE_ )

    allocate( PAGE_ , source = split(_CK_"foo; bar; baz", _CK_"; "))
    {%- for str in ["foo", "bar", "baz"] %}
    call assert_delayed ( PAGE_ ({{loop.index}}) == _CK_"{{str}}", __LINE__ )
    {%- endfor %}

    call assert_delayed ( join( PAGE_ , _CK_"; ") == _CK_"foo; bar; baz", __LINE__ , &
      "{{t.decl}} rejoin using '; ' test of 'foo bar baz' failed")
    call assert_delayed ( join( PAGE_ , _CK_"")   == _CK_"foobarbaz",     __LINE__ , &
      "{{t.decl}} rejoin w/ zero length string 'foo bar baz' failed")

    call assert_delayed ( join( _CK_"A line" // nl, _CK_" ending") == _CK_"A line ending", __LINE__ , &
      "{{t.decl}} join scalar")

    call assert_delayed( "ascii," // _CK_" maybe ascii" == _CK_"ascii, maybe ascii", &
      __LINE__ , "character(1) concat {{t.decl}}")

    {%- for ot in real_types %}
    call assert_delayed( _CK_"one = " // 1.0_{{ot.kind}} == _CK_"one = 1.00000", &
      __LINE__ , "{{t.decl}} concat {{ot.decl}}")
    call assert_delayed( 1.0_{{ot.kind}} // _CK_" = one" == _CK_"1.00000 = one", &
      __LINE__ , "{{ot.decl}} concat {{t.decl}}")
    {%- endfor %}

    {%- for ot in integer_types %}
    call assert_delayed( _CK_"one = " // 1_{{ot.kind}} == _CK_"one = 1", &
      __LINE__ , "{{t.decl}} concat {{ot.decl}}")
    call assert_delayed( 1_{{ot.kind}} // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "{{ot.decl}} concat {{t.decl}}")
    {%- endfor %}

    {%- for ot in logical_types %}
    block
      {{ot.decl}} :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "{{t.decl}} concat {{ot.decl}}")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "{{ot.decl}} concat {{t.decl}}")
    end block
    {%- endfor %}

    associate( msg => "{{t.decl}} conversion to integer failed")
      {%- for int in [-1, 1, -99, 999] %}
      call assert_delayed ( to_i( _CK_"{{int}}" )  == {{int}} , __LINE__ , msg)
      {%- endfor %}
    end associate

    associate( msg => "{{t.decl}} conversion to logical failed")
      call assert_delayed ( .not. to_l( _CK_"false"   ), __LINE__ , msg)
      call assert_delayed (       to_l( _CK_"true"    ), __LINE__ , msg)
      call assert_delayed ( .not. to_l( _CK_".false." ), __LINE__ , msg)
      call assert_delayed (       to_l( _CK_".true."  ), __LINE__ , msg)
    end associate

    associate( msg => "{{t.decl}} conversion to real failed")
      {%- for flt in ["0.0", "2.0", "-2.0", "2.0D0", "-2.0E0"] %}
      call assert_delayed ( to_r( _CK_"{{flt}}") == {{flt}},  __LINE__ , msg)
      {%- endfor %}
    end associate

  end associate
#undef PAGE_
#undef _CK_
#undef OUTPUT_
#undef STRING_EXPECT_
{% endfor %}

{% for t in integer_types %}
#define TK {{t.kind}}
  associate( msg => "{{t.decl}} to string conversion failed")
    {%- for int in [1, -1, 100, -100, 99, -99] %}
    call assert_delayed ( to_s({{int}}_ TK) == "{{int}}", __LINE__ , msg)
    {%- endfor %}
  end associate
#undef TK
{% endfor %}

{% for t in logical_types %}
  block
    {{t.decl}} :: bool
    bool = .true.
    call assert_delayed( to_s(bool) == "true",  __LINE__ , &
      "{{t.decl}} to string conversion failed" )
    call assert_delayed( bool , __LINE__ , &
      ".true. ({{t.decl}} passed to assert_delayed)")
    bool = .false.
    call assert_delayed( to_s(bool) == "false", __LINE__ , &
      "{{t.decl}} to string conversion failed")
    call assert_delayed( .not. bool , __LINE__ , &
      ".not. .false. ({{t.decl}} passed to assert_delayed)")
  end block
{% endfor %}

{% for t in real_types %}
  associate(msg => "unexpected value calling `to_s()` on {{t.decl}}")
    {%- for f, s in [
      ("0.0", "0.00000"),
      ("1.0", "1.00000"),
      ("-1.0", "-1.00000"),
      ("2.0", "2.00000"),
      ("-2.0", "-2.00000")
    ] %}
    call assert_delayed ( to_s({{f}}_{{t.kind}}) == "{{s}}", __LINE__, msg)
    {%- endfor %}
  end associate

  write(stdout,"(A)") ""
  {%- for prop in ["   tiny", "epsilon", "   huge"] %}
  write(stdout,"(A8,A8,A)") "{{t.decl}}", "{{prop}}", ": " // {{prop}}(1.0_{{t.kind}})
  {%- endfor %}
  write(stdout,"(A)") ""
{% endfor %}

!  call assert_delayed ( .false. , __LINE__ , "This should trigger a failure.")

  call pass_or_fail_test()

contains

  subroutine pass_or_fail_test()
    character(len=:), allocatable :: error_lines(:)
    integer :: i
    if(failures > 0) then
      allocate(error_lines, source = split(failed_lines, nl))
      write(stderr,"(A)") ""
      write(stderr,"(A)") "Detected " // &
        colorize(failures // " failure(s)", color_fg="red", style="bold_on") // &
        " of " // n_tests // " sub-test(s):"
      do i = 1, size(error_lines)
        write(stderr,"(A)") colorize("  " // THIS_FILE // ":" // error_lines(i), style="bold_on")
      end do
      write(stderr,"(A)") ""
      write(stderr,"(A)") colorize("Test failed!", color_fg="red", style="underline_on")
      write(stderr,"(A)") ""
      stop 1
    endif

    write(stdout,"(A)") ""
    write(stdout,"(A)") colorize("Test passed", color_fg="green_intense", style="bold_on") // " in:"
    write(stdout,"(A)") "  " // THIS_FILE // ","
    write(stdout,"(A)") "with " // to_s(n_tests) // " (100%) passing sub-tests."
    write(stdout,"(A)") ""

  end subroutine

  {% for lt in logical_types -%}
    subroutine assert_delayed_{{lt.alias}}(condition, line, message)
      {{lt.decl}}, intent(in) :: condition
      integer, intent(in) :: line
      character(len=*), intent(in), optional :: message
      character(len=:), allocatable :: temp

      n_tests = n_tests + 1
      if ( condition ) return ! Nothing to do here

      failures = failures + 1
      if ( present(message) ) write(stderr,"(A)") colorize(message, color_fg="yellow_intense", style="bold_on")
      if (.not. allocated(failed_lines)) then
        allocate(failed_lines, source = to_s(line))
      else
        allocate(temp, source = failed_lines // nl // to_s(line))
        deallocate(failed_lines)
        allocate(failed_lines, source=temp)
      end if
    end subroutine
  {% endfor -%}

  {% for t in character_types -%}
  subroutine string_expect_{{t.alias}}(input, expected)
    character(kind={{t.kind}},len=*), intent(in) :: input, expected

    if( input /= expected ) then
      failures = failures + 1
      write(*,"(A)",ADVANCE="NO") "Expected output: "
      write(*,"(A)") expected
      write(*,"(A)",ADVANCE="NO") "Actual output:    "
      write(*,"(A)") input
    end if
  end subroutine
  {% endfor -%}
end program
