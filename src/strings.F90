module zsfl_strings
  !! This module provides:
  !!   - Easy type conversion of intrinsic types to default character kind strings `to_s()`
  !!   - Easy type conversion of strings to default integers, reals, and logicals
  !!   - String manipulation functions inspired by Ruby & Python
  !!     - gsub: global substring substitution
  !!     - sub: single (left or right) substitution
  !!     - join: Join an array of strings (or a scalar with new-line separators) using the provided "glue"
  !!     - split: break a scalar string into an array of strings using the provided delimiter
  !!  - maybe_colorize: conditional formatting & colorization via ANSI control sequences
  !!  - String concatenation using all default scalar types (converted to character strings)

  use face, only: colorize
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env
  implicit none

  private
  public :: string_t, &
    {% for char in ["nl", "nul", "bel", "bsp", "tab", "spc"] -%}
      {{char}},
      {%- for t in character_types -%}
        {{char}}_{{t.alias}},
      {%- endfor %} &
    {% endfor -%}
    join, split, gsub, sub, &
    ascii_k, utf8_k, &
    operator(.join.), operator(.split.), operator(//), &
    to_i, to_l, to_r, to_s, init_float_fmt, &
    maybe_colorize, use_color, &
    bold, inverse, strikethrough, underline, &
    green, red, yellow

  integer, parameter :: ascii_k = selected_char_kind("ascii")
  integer, parameter :: utf8_k = selected_char_kind("ISO_10646")
  integer, parameter :: ck = ascii_k

  integer, parameter :: float_fmt_other = 9
  integer :: float_fmt_digits = 8
  character(len=:), allocatable :: float_fmt
  logical :: float_fmt_initialized = .false.
  logical :: color_on = .true.

  character(kind=ck,len=*), parameter :: nl = new_line(ck_"a"), &
    nul = achar(0,kind=ck), &
    bel = achar(7,kind=ck), &
    bsp = achar(8,kind=ck), &
    tab = achar(9,kind=ck), &
    spc = achar(32,kind=ck)
{% for t in character_types %}
  character(kind={{t.kind}}, len=*), parameter :: nl_{{t.alias}} = new_line({{t.kind}}_"a"), &
    nul_{{t.alias}} = achar(0,kind={{t.kind}}), &
    bel_{{t.alias}} = achar(7,kind={{t.kind}}), &
    bsp_{{t.alias}} = achar(8,kind={{t.kind}}), &
    tab_{{t.alias}} = achar(9,kind={{t.kind}}), &
    spc_{{t.alias}} = achar(32,kind={{t.kind}})
{% endfor %}
  type, abstract :: string_t

  contains
!    procedure, nopass :: split =>
      {%- for t in character_types -%}
        split_{{t.alias}}{{", " if not loop.last}}
      {%- endfor %}
!    procedure, nopass :: join =>
      {%- for t in character_types -%}
        join_{{t.alias}}{{", " if not loop.last}}
      {%- endfor %}
  end type

{% for proc in ["bold", "green", "gsub", "inverse", "maybe_colorize", "red", "strikethrough", "sub", "split", "underline", "yellow"] %}
  interface {{proc}}
  {%- for t in character_types %}
    module procedure {{proc}}_{{t.alias}}
  {%- endfor %}
  end interface
{%- endfor %}
  interface operator(.split.)
    {%- for t in character_types %}
    module procedure split_{{t.alias}}
    {%- endfor %}
  end interface operator(.split.)

{% for iface in ["join", "operator(.join.)"] %}
  interface {{iface}}
  {%- for proc in ["join_array", "join_scalar"] -%}
  {%- for t in character_types %}
    module procedure {{proc}}_{{t.alias}}
  {%- endfor %}
  {%- endfor %}
  end interface
{%- endfor %}

{% for iface in ["to_l", "operator(.toLogical.)"] %}
  interface {{iface}}
    {%- for ct in character_types %}
    module procedure conv_{{ct.alias}}_to_logical
    {%- endfor %}
  end interface
{%- endfor %}

{% for iface in ["to_i", "operator(.toInt.)"] %}
  interface {{iface}}
    {%- for ct in character_types %}
    module procedure conv_{{ct.alias}}_to_integer
    {%- endfor %}
  end interface
{%- endfor %}

{% for iface in ["to_r", "operator(.toReal.)"] %}
  interface {{iface}}
    {%- for ct in character_types %}
    module procedure conv_{{ct.alias}}_to_real
    {%- endfor %}
  end interface
{%- endfor %}

{% for iface in ["to_s", "operator(.toString.)"] %}
  interface {{iface}}
  {%- for t in logical_types + integer_types + real_types %}
    module procedure conv_{{t.alias}}_to_string
  {%- endfor %}
  end interface
{%- endfor %}

  interface operator(//)
    {%- for st in character_types %}
    {%- for ot in logical_types + real_types + integer_types %}
    module procedure {{st.alias}}_concat_{{ot.alias}}
    module procedure {{ot.alias}}_concat_{{st.alias}}
    {%- endfor %}
    {%- for ot in character_types if ot.kind < st.kind %}
    module procedure {{st.alias}}_concat_{{ot.alias}}
    module procedure {{ot.alias}}_concat_{{st.alias}}
    {%- endfor %}
    {%- endfor %}
  end interface operator(//)

contains

  subroutine init_float_fmt(significant_digits)
    integer, optional, intent(in) :: significant_digits

    if (.not. float_fmt_initialized) call set_float_fmt(significant_digits)
  end subroutine

  subroutine set_float_fmt(significant_digits)
    integer, optional, intent(in) :: significant_digits

    if ( present(significant_digits) ) float_fmt_digits = significant_digits
    allocate(float_fmt, source=("(g0." // to_s(float_fmt_digits) // ")"))
    float_fmt_initialized = .true.
  end subroutine

  subroutine use_color(true_false_predicate)
    logical, intent(in) :: true_false_predicate
    color_on = true_false_predicate
  end subroutine

{% for t in character_types %}

  pure function conv_{{t.alias}}_to_logical(input) result(res)
    !! Cast string to other intrinsic logical
    character(kind={{t.kind}},len=*), intent(in) :: input
    logical :: res
    read(input,*) res
  end function

  pure function conv_{{t.alias}}_to_integer(input) result(res)
    !! Cast string to other intrinsic integer
    character(kind={{t.kind}},len=*), intent(in) :: input
    integer(integer_kinds(size(integer_kinds))) :: res
    read(input,*) res
  end function

  pure function conv_{{t.alias}}_to_real(input) result(res)
    !! Cast string to other intrinsic real
    character(kind={{t.kind}},len=*), intent(in) :: input
    real(real_kinds(size(real_kinds))) :: res
    read(input,*) res
  end function


  pure function gsub_{{t.alias}}(string,search,replace) result(res)
    !! Global string substitution, inspired by ruby function
    character(kind={{t.kind}}, len=*), intent(in) :: string, search, replace
    character(kind={{t.kind}}, len=:), allocatable :: res, temp
    integer :: in_len, sub_len, replace_len, new_len, loc

    in_len = len_trim(string)
    sub_len = len_trim(search)
    replace_len = len_trim(replace)

    if (sub_len <= 0) return

    allocate(temp, source=string)
    do
      if(allocated(res)) deallocate(res)
      allocate(res, source=temp)

      ! Find first substring from left
      loc = index(res,trim(search))
      if ( loc == 0 ) then ! No more substrings to replace
        deallocate(temp)
        return
      else
        new_len = in_len - sub_len + replace_len
        deallocate(temp)
        allocate(character(kind={{t.kind}},len=new_len) :: temp)
        if(loc - 1 + sub_len < in_len) then ! Stuff after string being substituted
          temp = res(:loc-1) // replace(:replace_len) // res(loc + sub_len:)
        else
          temp = res(:loc-1) // replace(:replace_len)
        endif
        in_len = new_len
      endif
    end do
  end function

  pure function join_scalar_{{t.alias}}(string, glue) result(res)
    !! Scalar to scalar join: gsub on new line character
    character(kind={{t.kind}},len=*), intent(in) :: string, glue
    character(kind={{t.kind}},len=:), allocatable :: res

    res = gsub_{{t.alias}}(string, nl_{{t.alias}}, glue)
  end function

  pure function join_array_{{t.alias}}(string, glue) result(res)
    !! Plain array to scalar join: gsub on new line character
    character(kind={{t.kind}},len=*), intent(in) :: string(:), glue
    character(kind={{t.kind}},len=:), allocatable :: res
    integer :: i, slen

    slen = len_trim(string(1))
    do i = 2, size(string)
      slen = slen + len_trim(string(i)) + len(glue)
    end do
    allocate(character(kind={{t.kind}},len=slen) :: res)
    res(:) = trim(string(1))
    do i = 2, size(string)
      res(:) = trim(res(:)) // glue // trim(string(i))
    end do
  end function

  pure function maybe_colorize_{{t.alias}}(string, color_fg, color_bg, style) result(res)
    use face, only: colorize
    implicit none
    character(kind={{t.kind}}, len=*), intent(in) :: string
    character(len=*), intent(in), optional :: color_fg  !< Foreground color definition.
    character(len=*), intent(in), optional :: color_bg  !< Background color definition.
    character(len=*), intent(in), optional :: style     !< Style definition.
    character(kind={{t.kind}}, len=:), allocatable :: res

    if (color_on) then
      res = colorize(string, color_fg, color_bg, style)
    else
      res = string
    end if
  end function

{% for style in ["bold", "inverse", "strikethrough", "underline"] %}
  pure function {{style}}_{{t.alias}}(string) result(res)
    character(kind={{t.kind}}, len=*), intent(in) :: string
    character(kind={{t.kind}}, len=:), allocatable :: res
    res = maybe_colorize(string, style="{{style}}_on")
  end function
{% endfor %}

{% for color in ["green", "red", "yellow"] %}
  pure function {{color}}_{{t.alias}}(string) result(res)
    character(kind={{t.kind}}, len=*), intent(in) :: string
    character(kind={{t.kind}}, len=:), allocatable :: res
    res = maybe_colorize(string, color_fg="{{color}}_intense")
  end function
{% endfor %}

{% for ot in integer_types + real_types + logical_types %}
  pure function {{t.alias}}_concat_{{ot.alias}}(lhs, rhs) result(res)
    character(kind={{t.kind}},len=*), intent(in) :: lhs
    {{ot.decl}}, intent(in) :: rhs
    character(kind={{t.kind}},len=:), allocatable :: res
    character(kind={{t.kind}},len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind={{t.kind}},len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind={{t.kind}},len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function {{ot.alias}}_concat_{{t.alias}}(lhs, rhs) result(res)
    {{ot.decl}}, intent(in) :: lhs
    character(kind={{t.kind}},len=*), intent(in) :: rhs
    character(kind={{t.kind}},len=:), allocatable :: res
    character(kind={{t.kind}},len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind={{t.kind}},len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind={{t.kind}},len=(l1+l2)):: res)
    res = temp // rhs
  end function
{% endfor %}

{% for ot in character_types if ot.kind < t.kind %}
  pure function {{t.alias}}_concat_{{ot.alias}}(lhs,rhs) result(res)
    character(kind={{t.kind}},len=*), intent(in) :: lhs
    character(kind={{ot.kind}},len=*), intent(in) :: rhs
    character(kind={{t.kind}},len=:), allocatable :: res, temp
    integer :: l1, l2
    l1 = len(rhs)
    allocate(character(kind={{t.kind}},len=l1):: temp)
    temp = rhs
    l2 = len(lhs)
    allocate(character(kind={{t.kind}},len=(l1+l2)):: res)
    res = lhs // temp
  end function
  pure function {{ot.alias}}_concat_{{t.alias}}(lhs,rhs) result(res)
    character(kind={{ot.kind}},len=*), intent(in) :: lhs
    character(kind={{t.kind}},len=*), intent(in) :: rhs
    character(kind={{t.kind}},len=:), allocatable :: res, temp
    integer :: l1, l2
    l1 = len(lhs)
    allocate(character(kind={{t.kind}},len=l1) :: temp)
    temp = lhs
    l2 = len(rhs)
    allocate(character(kind={{t.kind}},len=(l1+l2)):: res)
    res = temp // rhs
  end function
{% endfor %}

  pure function split_{{t.alias}}(string,delimiter) result(res)
    character(kind={{t.kind}}, len=*), intent(in) :: string, delimiter
    character(kind={{t.kind}}, len=:), allocatable :: res(:)
    integer :: i, count, next, loc, max_size

    if (len(delimiter) == 0 ) then ! Nothing to do
      allocate(character(kind={{t.kind}},len=len(string)) :: res(1))
      res(1) = string
      return
    end if

    count = 0
    next = 1
    max_size = 0
    do
      count = count + 1
      loc = index(string(next:), delimiter)
      if (loc > 0) then
        max_size = max( loc-1, max_size )
        next = next + loc + len(delimiter) - 1
      else
        max_size = max( len(string(next:)), max_size)
        exit
      end if
    end do

    if (count == 0 ) then ! Nothing to do
      allocate(character(kind={{t.kind}},len=len(string)) :: res(1))
      res(1) = string
      return
    end if

    allocate(character(kind={{t.kind}},len=max_size) :: res(count))
    next = 1
    do i = 1,count-1
      loc = index(string(next:), delimiter) + next - 1
      res(i) = (string(next:loc-1))
      next = loc + len(delimiter)
    end do
    res(count) = (string(next:))

  end function

  pure function sub_{{t.alias}}(string,search,replace,back) result(res)
    !! Substitute first (or last) substring occurrence
    character(kind={{t.kind}}, len=*), intent(in) :: string, search, replace
    logical, optional, intent(in) :: back
    character(kind={{t.kind}}, len=:), allocatable :: res
    integer :: in_len, sub_len, replace_len, new_len, loc
    logical :: bck

    bck = .false.
    if(present(back)) bck = back
    in_len = len_trim(string)
    sub_len = len_trim(search)
    replace_len = len_trim(replace)

    if (sub_len <= 0) return

    ! Find first substring from left
    loc = index(string,trim(search),back=bck)
    if ( loc == 0 ) then ! No more substrings to replace
      allocate(res,source=string)
      return
    else
      new_len = in_len - sub_len + replace_len
      allocate(character(kind={{t.kind}},len=new_len) :: res)
      if(loc - 1 + sub_len < in_len) then ! Stuff after string being substituted
        res = string(:loc-1) // replace(:replace_len) // string(loc + sub_len:)
      else
        res = string(:loc-1) // replace(:replace_len)
      endif
    endif
  end function
{% endfor %}

{%- for t in logical_types %}
  pure function conv_{{t.alias}}_to_string(logical) result(res)
    !! Cast logical({{t.kind}}) variables as strings
    {{t.decl}}, intent(in) :: logical
    character(len=:), allocatable :: res

    if (logical) then
      allocate(res, source="true")
    else
      allocate(res, source="false")
    endif
  end function
{% endfor %}

{%- for t in integer_types %}
  pure function conv_{{t.alias}}_to_string(integer) result(res)
    !! Cast integer({{t.kind}}) variable to string
    {{t.decl}}, intent(in) :: integer
    character(len=:), allocatable :: res
    {{t.decl}} :: count, n

    count = 0_{{t.kind}}
    n = integer
    if (integer < 0) count = count + 1_{{t.kind}}
    do
      count = count + 1_{{t.kind}}
      n = n/10_{{t.kind}}
      if (n == 0_{{t.kind}}) exit
    end do
    allocate(character(len=count):: res)
    write(res,"(I0)") integer
  end function
{% endfor %}

{%- for t in real_types %}
  pure function conv_{{t.alias}}_to_string(input) result(res)
    !! Cast integer({{t.kind}}) variable to string
    {{t.decl}}, intent(in) :: input
    character(len=:), allocatable :: res, temp, my_fmt
    integer :: n_char

    if ( .not. float_fmt_initialized ) then
      allocate(my_fmt, source=("(g0." // to_s(float_fmt_digits) // ")"))
    else
      allocate(my_fmt, source=float_fmt)
    end if
    n_char = float_fmt_digits + float_fmt_other
    allocate(character(len=n_char):: temp)
    write(temp,trim(my_fmt)) input
    n_char = len_trim(temp)
    allocate(res, source=temp(:n_char))
    deallocate(temp)
  end function
{% endfor %}
end module
