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
    nl,nl_c1,nl_c4, &
    nul,nul_c1,nul_c4, &
    bel,bel_c1,bel_c4, &
    bsp,bsp_c1,bsp_c4, &
    tab,tab_c1,tab_c4, &
    spc,spc_c1,spc_c4, &
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

  character(kind=1, len=*), parameter :: nl_c1 = new_line(1_"a"), &
    nul_c1 = achar(0,kind=1), &
    bel_c1 = achar(7,kind=1), &
    bsp_c1 = achar(8,kind=1), &
    tab_c1 = achar(9,kind=1), &
    spc_c1 = achar(32,kind=1)

  character(kind=4, len=*), parameter :: nl_c4 = new_line(4_"a"), &
    nul_c4 = achar(0,kind=4), &
    bel_c4 = achar(7,kind=4), &
    bsp_c4 = achar(8,kind=4), &
    tab_c4 = achar(9,kind=4), &
    spc_c4 = achar(32,kind=4)

  type, abstract :: string_t

  contains
!    procedure, nopass :: split =>split_c1, split_c4
!    procedure, nopass :: join =>join_c1, join_c4
  end type


  interface bold
    module procedure bold_c1
    module procedure bold_c4
  end interface
  interface green
    module procedure green_c1
    module procedure green_c4
  end interface
  interface gsub
    module procedure gsub_c1
    module procedure gsub_c4
  end interface
  interface inverse
    module procedure inverse_c1
    module procedure inverse_c4
  end interface
  interface maybe_colorize
    module procedure maybe_colorize_c1
    module procedure maybe_colorize_c4
  end interface
  interface red
    module procedure red_c1
    module procedure red_c4
  end interface
  interface strikethrough
    module procedure strikethrough_c1
    module procedure strikethrough_c4
  end interface
  interface sub
    module procedure sub_c1
    module procedure sub_c4
  end interface
  interface split
    module procedure split_c1
    module procedure split_c4
  end interface
  interface underline
    module procedure underline_c1
    module procedure underline_c4
  end interface
  interface yellow
    module procedure yellow_c1
    module procedure yellow_c4
  end interface
  interface operator(.split.)
    module procedure split_c1
    module procedure split_c4
  end interface operator(.split.)


  interface join
    module procedure join_array_c1
    module procedure join_array_c4
    module procedure join_scalar_c1
    module procedure join_scalar_c4
  end interface
  interface operator(.join.)
    module procedure join_array_c1
    module procedure join_array_c4
    module procedure join_scalar_c1
    module procedure join_scalar_c4
  end interface


  interface to_l
    module procedure conv_c1_to_logical
    module procedure conv_c4_to_logical
  end interface
  interface operator(.toLogical.)
    module procedure conv_c1_to_logical
    module procedure conv_c4_to_logical
  end interface


  interface to_i
    module procedure conv_c1_to_integer
    module procedure conv_c4_to_integer
  end interface
  interface operator(.toInt.)
    module procedure conv_c1_to_integer
    module procedure conv_c4_to_integer
  end interface


  interface to_r
    module procedure conv_c1_to_real
    module procedure conv_c4_to_real
  end interface
  interface operator(.toReal.)
    module procedure conv_c1_to_real
    module procedure conv_c4_to_real
  end interface


  interface to_s
    module procedure conv_l1_to_string
    module procedure conv_l2_to_string
    module procedure conv_l4_to_string
    module procedure conv_l8_to_string
    module procedure conv_l16_to_string
    module procedure conv_i1_to_string
    module procedure conv_i2_to_string
    module procedure conv_i4_to_string
    module procedure conv_i8_to_string
    module procedure conv_i16_to_string
    module procedure conv_r4_to_string
    module procedure conv_r8_to_string
    module procedure conv_r10_to_string
    module procedure conv_r16_to_string
  end interface
  interface operator(.toString.)
    module procedure conv_l1_to_string
    module procedure conv_l2_to_string
    module procedure conv_l4_to_string
    module procedure conv_l8_to_string
    module procedure conv_l16_to_string
    module procedure conv_i1_to_string
    module procedure conv_i2_to_string
    module procedure conv_i4_to_string
    module procedure conv_i8_to_string
    module procedure conv_i16_to_string
    module procedure conv_r4_to_string
    module procedure conv_r8_to_string
    module procedure conv_r10_to_string
    module procedure conv_r16_to_string
  end interface

  interface operator(//)
    module procedure c1_concat_l1
    module procedure l1_concat_c1
    module procedure c1_concat_l2
    module procedure l2_concat_c1
    module procedure c1_concat_l4
    module procedure l4_concat_c1
    module procedure c1_concat_l8
    module procedure l8_concat_c1
    module procedure c1_concat_l16
    module procedure l16_concat_c1
    module procedure c1_concat_r4
    module procedure r4_concat_c1
    module procedure c1_concat_r8
    module procedure r8_concat_c1
    module procedure c1_concat_r10
    module procedure r10_concat_c1
    module procedure c1_concat_r16
    module procedure r16_concat_c1
    module procedure c1_concat_i1
    module procedure i1_concat_c1
    module procedure c1_concat_i2
    module procedure i2_concat_c1
    module procedure c1_concat_i4
    module procedure i4_concat_c1
    module procedure c1_concat_i8
    module procedure i8_concat_c1
    module procedure c1_concat_i16
    module procedure i16_concat_c1
    module procedure c4_concat_l1
    module procedure l1_concat_c4
    module procedure c4_concat_l2
    module procedure l2_concat_c4
    module procedure c4_concat_l4
    module procedure l4_concat_c4
    module procedure c4_concat_l8
    module procedure l8_concat_c4
    module procedure c4_concat_l16
    module procedure l16_concat_c4
    module procedure c4_concat_r4
    module procedure r4_concat_c4
    module procedure c4_concat_r8
    module procedure r8_concat_c4
    module procedure c4_concat_r10
    module procedure r10_concat_c4
    module procedure c4_concat_r16
    module procedure r16_concat_c4
    module procedure c4_concat_i1
    module procedure i1_concat_c4
    module procedure c4_concat_i2
    module procedure i2_concat_c4
    module procedure c4_concat_i4
    module procedure i4_concat_c4
    module procedure c4_concat_i8
    module procedure i8_concat_c4
    module procedure c4_concat_i16
    module procedure i16_concat_c4
    module procedure c4_concat_c1
    module procedure c1_concat_c4
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



  pure function conv_c1_to_logical(input) result(res)
    !! Cast string to other intrinsic logical
    character(kind=1,len=*), intent(in) :: input
    logical :: res
    read(input,*) res
  end function

  pure function conv_c1_to_integer(input) result(res)
    !! Cast string to other intrinsic integer
    character(kind=1,len=*), intent(in) :: input
    integer(integer_kinds(size(integer_kinds))) :: res
    read(input,*) res
  end function

  pure function conv_c1_to_real(input) result(res)
    !! Cast string to other intrinsic real
    character(kind=1,len=*), intent(in) :: input
    real(real_kinds(size(real_kinds))) :: res
    read(input,*) res
  end function


  pure function gsub_c1(string,search,replace) result(res)
    !! Global string substitution, inspired by ruby function
    character(kind=1, len=*), intent(in) :: string, search, replace
    character(kind=1, len=:), allocatable :: res, temp
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
        allocate(character(kind=1,len=new_len) :: temp)
        if(loc - 1 + sub_len < in_len) then ! Stuff after string being substituted
          temp = res(:loc-1) // replace(:replace_len) // res(loc + sub_len:)
        else
          temp = res(:loc-1) // replace(:replace_len)
        endif
        in_len = new_len
      endif
    end do
  end function

  pure function join_scalar_c1(string, glue) result(res)
    !! Scalar to scalar join: gsub on new line character
    character(kind=1,len=*), intent(in) :: string, glue
    character(kind=1,len=:), allocatable :: res

    res = gsub_c1(string, nl_c1, glue)
  end function

  pure function join_array_c1(string, glue) result(res)
    !! Plain array to scalar join: gsub on new line character
    character(kind=1,len=*), intent(in) :: string(:), glue
    character(kind=1,len=:), allocatable :: res
    integer :: i, slen

    slen = len_trim(string(1))
    do i = 2, size(string)
      slen = slen + len_trim(string(i)) + len(glue)
    end do
    allocate(character(kind=1,len=slen) :: res)
    res(:) = trim(string(1))
    do i = 2, size(string)
      res(:) = trim(res(:)) // glue // trim(string(i))
    end do
  end function

  pure function maybe_colorize_c1(string, color_fg, color_bg, style) result(res)
    use face, only: colorize
    implicit none
    character(kind=1, len=*), intent(in) :: string
    character(len=*), intent(in), optional :: color_fg  !< Foreground color definition.
    character(len=*), intent(in), optional :: color_bg  !< Background color definition.
    character(len=*), intent(in), optional :: style     !< Style definition.
    character(kind=1, len=:), allocatable :: res

    if (color_on) then
      res = colorize(string, color_fg, color_bg, style)
    else
      res = string
    end if
  end function


  pure function bold_c1(string) result(res)
    character(kind=1, len=*), intent(in) :: string
    character(kind=1, len=:), allocatable :: res
    res = maybe_colorize(string, style="bold_on")
  end function

  pure function inverse_c1(string) result(res)
    character(kind=1, len=*), intent(in) :: string
    character(kind=1, len=:), allocatable :: res
    res = maybe_colorize(string, style="inverse_on")
  end function

  pure function strikethrough_c1(string) result(res)
    character(kind=1, len=*), intent(in) :: string
    character(kind=1, len=:), allocatable :: res
    res = maybe_colorize(string, style="strikethrough_on")
  end function

  pure function underline_c1(string) result(res)
    character(kind=1, len=*), intent(in) :: string
    character(kind=1, len=:), allocatable :: res
    res = maybe_colorize(string, style="underline_on")
  end function



  pure function green_c1(string) result(res)
    character(kind=1, len=*), intent(in) :: string
    character(kind=1, len=:), allocatable :: res
    res = maybe_colorize(string, color_fg="green_intense")
  end function

  pure function red_c1(string) result(res)
    character(kind=1, len=*), intent(in) :: string
    character(kind=1, len=:), allocatable :: res
    res = maybe_colorize(string, color_fg="red_intense")
  end function

  pure function yellow_c1(string) result(res)
    character(kind=1, len=*), intent(in) :: string
    character(kind=1, len=:), allocatable :: res
    res = maybe_colorize(string, color_fg="yellow_intense")
  end function



  pure function c1_concat_i1(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    integer(1), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i1_concat_c1(lhs, rhs) result(res)
    integer(1), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_i2(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    integer(2), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i2_concat_c1(lhs, rhs) result(res)
    integer(2), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_i4(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    integer(4), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i4_concat_c1(lhs, rhs) result(res)
    integer(4), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_i8(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    integer(8), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i8_concat_c1(lhs, rhs) result(res)
    integer(8), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_i16(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    integer(16), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i16_concat_c1(lhs, rhs) result(res)
    integer(16), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_r4(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    real(4), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function r4_concat_c1(lhs, rhs) result(res)
    real(4), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_r8(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    real(8), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function r8_concat_c1(lhs, rhs) result(res)
    real(8), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_r10(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    real(10), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function r10_concat_c1(lhs, rhs) result(res)
    real(10), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_r16(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    real(16), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function r16_concat_c1(lhs, rhs) result(res)
    real(16), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_l1(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    logical(1), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l1_concat_c1(lhs, rhs) result(res)
    logical(1), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_l2(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    logical(2), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l2_concat_c1(lhs, rhs) result(res)
    logical(2), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_l4(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    logical(4), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l4_concat_c1(lhs, rhs) result(res)
    logical(4), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_l8(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    logical(8), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l8_concat_c1(lhs, rhs) result(res)
    logical(8), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c1_concat_l16(lhs, rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    logical(16), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=1,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l16_concat_c1(lhs, rhs) result(res)
    logical(16), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=1,len=:), allocatable :: res
    character(kind=1,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=1,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=1,len=(l1+l2)):: res)
    res = temp // rhs
  end function




  pure function split_c1(string,delimiter) result(res)
    character(kind=1, len=*), intent(in) :: string, delimiter
    character(kind=1, len=:), allocatable :: res(:)
    integer :: i, count, next, loc, max_size

    if (len(delimiter) == 0 ) then ! Nothing to do
      allocate(character(kind=1,len=len(string)) :: res(1))
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

    allocate(character(kind=1,len=max_size) :: res(count))
    next = 1
    do i = 1,count-1
      loc = index(string(next:), delimiter) + next - 1
      res(i) = (string(next:loc-1))
      next = loc + len(delimiter)
    end do
    res(count) = (string(next:))

  end function

  pure function sub_c1(string,search,replace,back) result(res)
    !! Substitute first (or last) substring occurrence
    character(kind=1, len=*), intent(in) :: string, search, replace
    logical, optional, intent(in) :: back
    character(kind=1, len=:), allocatable :: res
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
      allocate(character(kind=1,len=new_len) :: res)
      if(loc - 1 + sub_len < in_len) then ! Stuff after string being substituted
        res = string(:loc-1) // replace(:replace_len) // string(loc + sub_len:)
      else
        res = string(:loc-1) // replace(:replace_len)
      endif
    endif
  end function


  pure function conv_c4_to_logical(input) result(res)
    !! Cast string to other intrinsic logical
    character(kind=4,len=*), intent(in) :: input
    logical :: res
    read(input,*) res
  end function

  pure function conv_c4_to_integer(input) result(res)
    !! Cast string to other intrinsic integer
    character(kind=4,len=*), intent(in) :: input
    integer(integer_kinds(size(integer_kinds))) :: res
    read(input,*) res
  end function

  pure function conv_c4_to_real(input) result(res)
    !! Cast string to other intrinsic real
    character(kind=4,len=*), intent(in) :: input
    real(real_kinds(size(real_kinds))) :: res
    read(input,*) res
  end function


  pure function gsub_c4(string,search,replace) result(res)
    !! Global string substitution, inspired by ruby function
    character(kind=4, len=*), intent(in) :: string, search, replace
    character(kind=4, len=:), allocatable :: res, temp
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
        allocate(character(kind=4,len=new_len) :: temp)
        if(loc - 1 + sub_len < in_len) then ! Stuff after string being substituted
          temp = res(:loc-1) // replace(:replace_len) // res(loc + sub_len:)
        else
          temp = res(:loc-1) // replace(:replace_len)
        endif
        in_len = new_len
      endif
    end do
  end function

  pure function join_scalar_c4(string, glue) result(res)
    !! Scalar to scalar join: gsub on new line character
    character(kind=4,len=*), intent(in) :: string, glue
    character(kind=4,len=:), allocatable :: res

    res = gsub_c4(string, nl_c4, glue)
  end function

  pure function join_array_c4(string, glue) result(res)
    !! Plain array to scalar join: gsub on new line character
    character(kind=4,len=*), intent(in) :: string(:), glue
    character(kind=4,len=:), allocatable :: res
    integer :: i, slen

    slen = len_trim(string(1))
    do i = 2, size(string)
      slen = slen + len_trim(string(i)) + len(glue)
    end do
    allocate(character(kind=4,len=slen) :: res)
    res(:) = trim(string(1))
    do i = 2, size(string)
      res(:) = trim(res(:)) // glue // trim(string(i))
    end do
  end function

  pure function maybe_colorize_c4(string, color_fg, color_bg, style) result(res)
    use face, only: colorize
    implicit none
    character(kind=4, len=*), intent(in) :: string
    character(len=*), intent(in), optional :: color_fg  !< Foreground color definition.
    character(len=*), intent(in), optional :: color_bg  !< Background color definition.
    character(len=*), intent(in), optional :: style     !< Style definition.
    character(kind=4, len=:), allocatable :: res

    if (color_on) then
      res = colorize(string, color_fg, color_bg, style)
    else
      res = string
    end if
  end function


  pure function bold_c4(string) result(res)
    character(kind=4, len=*), intent(in) :: string
    character(kind=4, len=:), allocatable :: res
    res = maybe_colorize(string, style="bold_on")
  end function

  pure function inverse_c4(string) result(res)
    character(kind=4, len=*), intent(in) :: string
    character(kind=4, len=:), allocatable :: res
    res = maybe_colorize(string, style="inverse_on")
  end function

  pure function strikethrough_c4(string) result(res)
    character(kind=4, len=*), intent(in) :: string
    character(kind=4, len=:), allocatable :: res
    res = maybe_colorize(string, style="strikethrough_on")
  end function

  pure function underline_c4(string) result(res)
    character(kind=4, len=*), intent(in) :: string
    character(kind=4, len=:), allocatable :: res
    res = maybe_colorize(string, style="underline_on")
  end function



  pure function green_c4(string) result(res)
    character(kind=4, len=*), intent(in) :: string
    character(kind=4, len=:), allocatable :: res
    res = maybe_colorize(string, color_fg="green_intense")
  end function

  pure function red_c4(string) result(res)
    character(kind=4, len=*), intent(in) :: string
    character(kind=4, len=:), allocatable :: res
    res = maybe_colorize(string, color_fg="red_intense")
  end function

  pure function yellow_c4(string) result(res)
    character(kind=4, len=*), intent(in) :: string
    character(kind=4, len=:), allocatable :: res
    res = maybe_colorize(string, color_fg="yellow_intense")
  end function



  pure function c4_concat_i1(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    integer(1), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i1_concat_c4(lhs, rhs) result(res)
    integer(1), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_i2(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    integer(2), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i2_concat_c4(lhs, rhs) result(res)
    integer(2), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_i4(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    integer(4), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i4_concat_c4(lhs, rhs) result(res)
    integer(4), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_i8(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    integer(8), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i8_concat_c4(lhs, rhs) result(res)
    integer(8), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_i16(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    integer(16), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function i16_concat_c4(lhs, rhs) result(res)
    integer(16), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_r4(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    real(4), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function r4_concat_c4(lhs, rhs) result(res)
    real(4), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_r8(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    real(8), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function r8_concat_c4(lhs, rhs) result(res)
    real(8), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_r10(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    real(10), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function r10_concat_c4(lhs, rhs) result(res)
    real(10), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_r16(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    real(16), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function r16_concat_c4(lhs, rhs) result(res)
    real(16), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_l1(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    logical(1), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l1_concat_c4(lhs, rhs) result(res)
    logical(1), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_l2(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    logical(2), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l2_concat_c4(lhs, rhs) result(res)
    logical(2), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_l4(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    logical(4), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l4_concat_c4(lhs, rhs) result(res)
    logical(4), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_l8(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    logical(8), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l8_concat_c4(lhs, rhs) result(res)
    logical(8), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function

  pure function c4_concat_l16(lhs, rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    logical(16), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(rhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(rhs)
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)) :: res)
    res = lhs // temp
  end function

  pure function l16_concat_c4(lhs, rhs) result(res)
    logical(16), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res
    character(kind=4,len=:), allocatable :: temp
    integer :: l1, l2

    l1 = len(to_s(lhs))
    allocate(character(kind=4,len=l1) :: temp)
    temp = to_s(lhs)
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function



  pure function c4_concat_c1(lhs,rhs) result(res)
    character(kind=4,len=*), intent(in) :: lhs
    character(kind=1,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res, temp
    integer :: l1, l2
    l1 = len(rhs)
    allocate(character(kind=4,len=l1):: temp)
    temp = rhs
    l2 = len(lhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = lhs // temp
  end function
  pure function c1_concat_c4(lhs,rhs) result(res)
    character(kind=1,len=*), intent(in) :: lhs
    character(kind=4,len=*), intent(in) :: rhs
    character(kind=4,len=:), allocatable :: res, temp
    integer :: l1, l2
    l1 = len(lhs)
    allocate(character(kind=4,len=l1) :: temp)
    temp = lhs
    l2 = len(rhs)
    allocate(character(kind=4,len=(l1+l2)):: res)
    res = temp // rhs
  end function


  pure function split_c4(string,delimiter) result(res)
    character(kind=4, len=*), intent(in) :: string, delimiter
    character(kind=4, len=:), allocatable :: res(:)
    integer :: i, count, next, loc, max_size

    if (len(delimiter) == 0 ) then ! Nothing to do
      allocate(character(kind=4,len=len(string)) :: res(1))
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

    allocate(character(kind=4,len=max_size) :: res(count))
    next = 1
    do i = 1,count-1
      loc = index(string(next:), delimiter) + next - 1
      res(i) = (string(next:loc-1))
      next = loc + len(delimiter)
    end do
    res(count) = (string(next:))

  end function

  pure function sub_c4(string,search,replace,back) result(res)
    !! Substitute first (or last) substring occurrence
    character(kind=4, len=*), intent(in) :: string, search, replace
    logical, optional, intent(in) :: back
    character(kind=4, len=:), allocatable :: res
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
      allocate(character(kind=4,len=new_len) :: res)
      if(loc - 1 + sub_len < in_len) then ! Stuff after string being substituted
        res = string(:loc-1) // replace(:replace_len) // string(loc + sub_len:)
      else
        res = string(:loc-1) // replace(:replace_len)
      endif
    endif
  end function

  pure function conv_l1_to_string(logical) result(res)
    !! Cast logical(1) variables as strings
    logical(1), intent(in) :: logical
    character(len=:), allocatable :: res

    if (logical) then
      allocate(res, source="true")
    else
      allocate(res, source="false")
    endif
  end function

  pure function conv_l2_to_string(logical) result(res)
    !! Cast logical(2) variables as strings
    logical(2), intent(in) :: logical
    character(len=:), allocatable :: res

    if (logical) then
      allocate(res, source="true")
    else
      allocate(res, source="false")
    endif
  end function

  pure function conv_l4_to_string(logical) result(res)
    !! Cast logical(4) variables as strings
    logical(4), intent(in) :: logical
    character(len=:), allocatable :: res

    if (logical) then
      allocate(res, source="true")
    else
      allocate(res, source="false")
    endif
  end function

  pure function conv_l8_to_string(logical) result(res)
    !! Cast logical(8) variables as strings
    logical(8), intent(in) :: logical
    character(len=:), allocatable :: res

    if (logical) then
      allocate(res, source="true")
    else
      allocate(res, source="false")
    endif
  end function

  pure function conv_l16_to_string(logical) result(res)
    !! Cast logical(16) variables as strings
    logical(16), intent(in) :: logical
    character(len=:), allocatable :: res

    if (logical) then
      allocate(res, source="true")
    else
      allocate(res, source="false")
    endif
  end function

  pure function conv_i1_to_string(integer) result(res)
    !! Cast integer(1) variable to string
    integer(1), intent(in) :: integer
    character(len=:), allocatable :: res
    integer(1) :: count, n

    count = 0_1
    n = integer
    if (integer < 0) count = count + 1_1
    do
      count = count + 1_1
      n = n/10_1
      if (n == 0_1) exit
    end do
    allocate(character(len=count):: res)
    write(res,"(I0)") integer
  end function

  pure function conv_i2_to_string(integer) result(res)
    !! Cast integer(2) variable to string
    integer(2), intent(in) :: integer
    character(len=:), allocatable :: res
    integer(2) :: count, n

    count = 0_2
    n = integer
    if (integer < 0) count = count + 1_2
    do
      count = count + 1_2
      n = n/10_2
      if (n == 0_2) exit
    end do
    allocate(character(len=count):: res)
    write(res,"(I0)") integer
  end function

  pure function conv_i4_to_string(integer) result(res)
    !! Cast integer(4) variable to string
    integer(4), intent(in) :: integer
    character(len=:), allocatable :: res
    integer(4) :: count, n

    count = 0_4
    n = integer
    if (integer < 0) count = count + 1_4
    do
      count = count + 1_4
      n = n/10_4
      if (n == 0_4) exit
    end do
    allocate(character(len=count):: res)
    write(res,"(I0)") integer
  end function

  pure function conv_i8_to_string(integer) result(res)
    !! Cast integer(8) variable to string
    integer(8), intent(in) :: integer
    character(len=:), allocatable :: res
    integer(8) :: count, n

    count = 0_8
    n = integer
    if (integer < 0) count = count + 1_8
    do
      count = count + 1_8
      n = n/10_8
      if (n == 0_8) exit
    end do
    allocate(character(len=count):: res)
    write(res,"(I0)") integer
  end function

  pure function conv_i16_to_string(integer) result(res)
    !! Cast integer(16) variable to string
    integer(16), intent(in) :: integer
    character(len=:), allocatable :: res
    integer(16) :: count, n

    count = 0_16
    n = integer
    if (integer < 0) count = count + 1_16
    do
      count = count + 1_16
      n = n/10_16
      if (n == 0_16) exit
    end do
    allocate(character(len=count):: res)
    write(res,"(I0)") integer
  end function

  pure function conv_r4_to_string(input) result(res)
    !! Cast integer(4) variable to string
    real(4), intent(in) :: input
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

  pure function conv_r8_to_string(input) result(res)
    !! Cast integer(8) variable to string
    real(8), intent(in) :: input
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

  pure function conv_r10_to_string(input) result(res)
    !! Cast integer(10) variable to string
    real(10), intent(in) :: input
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

  pure function conv_r16_to_string(input) result(res)
    !! Cast integer(16) variable to string
    real(16), intent(in) :: input
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

end module
