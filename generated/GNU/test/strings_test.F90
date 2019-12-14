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

  character(kind=1,len=*), parameter :: &
    qbf_c1 = 1_"the quick brown fox jumped over the lazy dog"
  character(kind=1,len=*), parameter :: &
    paths_c1 = 1_"/bin:/sbin:/usr/bin:/usr/local/bin"
  character(kind=1,len=:), allocatable :: output_c1
  character(kind=1,len=:), allocatable :: page_c1(:)

  character(kind=4,len=*), parameter :: &
    qbf_c4 = 4_"the quick brown fox jumped over the lazy dog"
  character(kind=4,len=*), parameter :: &
    paths_c4 = 4_"/bin:/sbin:/usr/bin:/usr/local/bin"
  character(kind=4,len=:), allocatable :: output_c4
  character(kind=4,len=:), allocatable :: page_c4(:)

  integer :: i, failures = 0, n_tests = 0
  character(len=:), allocatable :: failed_lines


  interface string_expect
    procedure string_expect_c1
    procedure string_expect_c4
  end interface

  interface assert_delayed
    procedure assert_delayed_l1
    procedure assert_delayed_l2
    procedure assert_delayed_l4
    procedure assert_delayed_l8
    procedure assert_delayed_l16
  end interface


call init_float_fmt(6)
call use_color(.true.)

!  call use_color(.false.)
  write(stdout,"(A)") ""
  write(stdout,"(A,I0)") "Default kind = ", selected_char_kind("default")
  write(stdout,"(A,I0)") "Ascii kind   = ", ascii_k
  write(stdout,"(A,I0)") "Unicode kind = ", utf8_k
  ! call init_float_fmt()


#define PAGE_ page_c1
#define _CK_ 1_
#define OUTPUT_ output_c1

  write(stdout,"(A)") yellow(_CK_"Testing " //underline(_CK_"c1"))
  write(stdout,"(A)") bold(_CK_"This text is "//green(_CK_"green"))
  write(stdout,"(A)") red(_CK_"This text is "//strikethrough(_CK_"not")//_CK_" red")
  write(stdout,"(A)") inverse(_CK_"Inverse text.")

  associate(paths => paths_c1, qbf => qbf_c1)
    call use_color(.false.)
    OUTPUT_ = underline( yellow(qbf))
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "underline( yellow( character(1) )) (color = OFF) failed")
    OUTPUT_ = bold( green(qbf))
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "bold( green( character(1) )) (color = OFF) failed")
    OUTPUT_ = red( strikethrough(qbf))
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "red( strikethrough( character(1) )) (color = OFF) failed")
    OUTPUT_ = inverse(qbf)
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "inverse( character(1) ) (color = OFF) failed")
    call use_color(.true.)

    OUTPUT_ = gsub(qbf, _CK_"the", _CK_"a")
    call assert_delayed ( &
      OUTPUT_ ==  _CK_"a quick brown fox jumped over a lazy dog", __LINE__ , &
      "character(1) gsub 'the --> a' substitution failed")

    call assert_delayed ( gsub(qbf, _CK_"moose", _CK_"fox") == qbf, __LINE__ , &
      "character(1) gsub no-op failed")

    OUTPUT_ = gsub(qbf, _CK_"dog", _CK_"god")
    call assert_delayed ( &
      OUTPUT_ == _CK_"the quick brown fox jumped over the lazy god", __LINE__ , &
      "character(1) gsub 'dog --> god' failed")

    OUTPUT_ = sub(qbf, _CK_"dog", _CK_"god")
    call assert_delayed ( &
      OUTPUT_ == _CK_"the quick brown fox jumped over the lazy god", __LINE__ , &
      "character(1) sub 'dog --> god' failed")

    OUTPUT_ = sub(qbf, _CK_"the", _CK_"a")
    call assert_delayed ( &
      OUTPUT_ == _CK_"a quick brown fox jumped over the lazy dog", __LINE__ , &
      "character(1) sub (left) 'the --> a' failed")

    OUTPUT_ = sub(qbf, _CK_"the", _CK_"a", back=.true.)
    call assert_delayed ( &
      OUTPUT_ == _CK_"the quick brown fox jumped over a lazy dog", __LINE__ , &
      "character(1) sub (right) 'the --> a' failed")

    OUTPUT_ = sub(qbf, _CK_"chicken", _CK_"rooster")
    call assert_delayed ( &
      OUTPUT_ == qbf, __LINE__ , &
      "character(1) sub no-op failed")

    allocate( PAGE_ , source = split(paths, _CK_":"))
    associate(msg => "character(1) path splitting test failed")
      call assert_delayed ( PAGE_ (1) == _CK_"/bin", __LINE__, msg)
      call assert_delayed ( PAGE_ (2) == _CK_"/sbin", __LINE__, msg)
      call assert_delayed ( PAGE_ (3) == _CK_"/usr/bin", __LINE__, msg)
      call assert_delayed ( PAGE_ (4) == _CK_"/usr/local/bin", __LINE__, msg)
    end associate

    deallocate( PAGE_ )
    allocate( PAGE_ , source = split(paths, _CK_""))
    do i = 1, size( PAGE_ )
      call assert_delayed ( PAGE_ (i) == paths, __LINE__ , &
        "character(1) no-op split on empty string test failed")
    end do

    deallocate( PAGE_ )
    allocate( PAGE_ , source = split(paths, _CK_";"))
    do i = 1, size( PAGE_ )
      call assert_delayed ( PAGE_ (i) == paths, __LINE__ , &
        "character(1) no-op split test failed")
    end do
    deallocate( PAGE_ )

    allocate( PAGE_ , source = split(_CK_"foo; bar; baz", _CK_"; "))
    call assert_delayed ( PAGE_ (1) == _CK_"foo", __LINE__ )
    call assert_delayed ( PAGE_ (2) == _CK_"bar", __LINE__ )
    call assert_delayed ( PAGE_ (3) == _CK_"baz", __LINE__ )

    call assert_delayed ( join( PAGE_ , _CK_"; ") == _CK_"foo; bar; baz", __LINE__ , &
      "character(1) rejoin using '; ' test of 'foo bar baz' failed")
    call assert_delayed ( join( PAGE_ , _CK_"")   == _CK_"foobarbaz",     __LINE__ , &
      "character(1) rejoin w/ zero length string 'foo bar baz' failed")

    call assert_delayed ( join( _CK_"A line" // nl, _CK_" ending") == _CK_"A line ending", __LINE__ , &
      "character(1) join scalar")

    call assert_delayed( "ascii," // _CK_" maybe ascii" == _CK_"ascii, maybe ascii", &
      __LINE__ , "character(1) concat character(1)")
    call assert_delayed( _CK_"one = " // 1.0_4 == _CK_"one = 1.00000", &
      __LINE__ , "character(1) concat real(4)")
    call assert_delayed( 1.0_4 // _CK_" = one" == _CK_"1.00000 = one", &
      __LINE__ , "real(4) concat character(1)")
    call assert_delayed( _CK_"one = " // 1.0_8 == _CK_"one = 1.00000", &
      __LINE__ , "character(1) concat real(8)")
    call assert_delayed( 1.0_8 // _CK_" = one" == _CK_"1.00000 = one", &
      __LINE__ , "real(8) concat character(1)")
    call assert_delayed( _CK_"one = " // 1.0_10 == _CK_"one = 1.00000", &
      __LINE__ , "character(1) concat real(10)")
    call assert_delayed( 1.0_10 // _CK_" = one" == _CK_"1.00000 = one", &
      __LINE__ , "real(10) concat character(1)")
    call assert_delayed( _CK_"one = " // 1.0_16 == _CK_"one = 1.00000", &
      __LINE__ , "character(1) concat real(16)")
    call assert_delayed( 1.0_16 // _CK_" = one" == _CK_"1.00000 = one", &
      __LINE__ , "real(16) concat character(1)")
    call assert_delayed( _CK_"one = " // 1_1 == _CK_"one = 1", &
      __LINE__ , "character(1) concat integer(1)")
    call assert_delayed( 1_1 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(1) concat character(1)")
    call assert_delayed( _CK_"one = " // 1_2 == _CK_"one = 1", &
      __LINE__ , "character(1) concat integer(2)")
    call assert_delayed( 1_2 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(2) concat character(1)")
    call assert_delayed( _CK_"one = " // 1_4 == _CK_"one = 1", &
      __LINE__ , "character(1) concat integer(4)")
    call assert_delayed( 1_4 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(4) concat character(1)")
    call assert_delayed( _CK_"one = " // 1_8 == _CK_"one = 1", &
      __LINE__ , "character(1) concat integer(8)")
    call assert_delayed( 1_8 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(8) concat character(1)")
    call assert_delayed( _CK_"one = " // 1_16 == _CK_"one = 1", &
      __LINE__ , "character(1) concat integer(16)")
    call assert_delayed( 1_16 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(16) concat character(1)")
    block
      logical(1) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(1) concat logical(1)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(1) concat character(1)")
    end block
    block
      logical(2) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(1) concat logical(2)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(2) concat character(1)")
    end block
    block
      logical(4) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(1) concat logical(4)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(4) concat character(1)")
    end block
    block
      logical(8) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(1) concat logical(8)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(8) concat character(1)")
    end block
    block
      logical(16) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(1) concat logical(16)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(16) concat character(1)")
    end block

    associate( msg => "character(1) conversion to integer failed")
      call assert_delayed ( to_i( _CK_"-1" )  == -1 , __LINE__ , msg)
      call assert_delayed ( to_i( _CK_"1" )  == 1 , __LINE__ , msg)
      call assert_delayed ( to_i( _CK_"-99" )  == -99 , __LINE__ , msg)
      call assert_delayed ( to_i( _CK_"999" )  == 999 , __LINE__ , msg)
    end associate

    associate( msg => "character(1) conversion to logical failed")
      call assert_delayed ( .not. to_l( _CK_"false"   ), __LINE__ , msg)
      call assert_delayed (       to_l( _CK_"true"    ), __LINE__ , msg)
      call assert_delayed ( .not. to_l( _CK_".false." ), __LINE__ , msg)
      call assert_delayed (       to_l( _CK_".true."  ), __LINE__ , msg)
    end associate

    associate( msg => "character(1) conversion to real failed")
      call assert_delayed ( to_r( _CK_"0.0") == 0.0,  __LINE__ , msg)
      call assert_delayed ( to_r( _CK_"2.0") == 2.0,  __LINE__ , msg)
      call assert_delayed ( to_r( _CK_"-2.0") == -2.0,  __LINE__ , msg)
      call assert_delayed ( to_r( _CK_"2.0D0") == 2.0D0,  __LINE__ , msg)
      call assert_delayed ( to_r( _CK_"-2.0E0") == -2.0E0,  __LINE__ , msg)
    end associate

  end associate
#undef PAGE_
#undef _CK_
#undef OUTPUT_
#undef STRING_EXPECT_

#define PAGE_ page_c4
#define _CK_ 4_
#define OUTPUT_ output_c4

  write(stdout,"(A)") yellow(_CK_"Testing " //underline(_CK_"c4"))
  write(stdout,"(A)") bold(_CK_"This text is "//green(_CK_"green"))
  write(stdout,"(A)") red(_CK_"This text is "//strikethrough(_CK_"not")//_CK_" red")
  write(stdout,"(A)") inverse(_CK_"Inverse text.")

  associate(paths => paths_c4, qbf => qbf_c4)
    call use_color(.false.)
    OUTPUT_ = underline( yellow(qbf))
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "underline( yellow( character(4) )) (color = OFF) failed")
    OUTPUT_ = bold( green(qbf))
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "bold( green( character(4) )) (color = OFF) failed")
    OUTPUT_ = red( strikethrough(qbf))
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "red( strikethrough( character(4) )) (color = OFF) failed")
    OUTPUT_ = inverse(qbf)
    call assert_delayed ( OUTPUT_ == qbf , __LINE__ , &
      "inverse( character(4) ) (color = OFF) failed")
    call use_color(.true.)

    OUTPUT_ = gsub(qbf, _CK_"the", _CK_"a")
    call assert_delayed ( &
      OUTPUT_ ==  _CK_"a quick brown fox jumped over a lazy dog", __LINE__ , &
      "character(4) gsub 'the --> a' substitution failed")

    call assert_delayed ( gsub(qbf, _CK_"moose", _CK_"fox") == qbf, __LINE__ , &
      "character(4) gsub no-op failed")

    OUTPUT_ = gsub(qbf, _CK_"dog", _CK_"god")
    call assert_delayed ( &
      OUTPUT_ == _CK_"the quick brown fox jumped over the lazy god", __LINE__ , &
      "character(4) gsub 'dog --> god' failed")

    OUTPUT_ = sub(qbf, _CK_"dog", _CK_"god")
    call assert_delayed ( &
      OUTPUT_ == _CK_"the quick brown fox jumped over the lazy god", __LINE__ , &
      "character(4) sub 'dog --> god' failed")

    OUTPUT_ = sub(qbf, _CK_"the", _CK_"a")
    call assert_delayed ( &
      OUTPUT_ == _CK_"a quick brown fox jumped over the lazy dog", __LINE__ , &
      "character(4) sub (left) 'the --> a' failed")

    OUTPUT_ = sub(qbf, _CK_"the", _CK_"a", back=.true.)
    call assert_delayed ( &
      OUTPUT_ == _CK_"the quick brown fox jumped over a lazy dog", __LINE__ , &
      "character(4) sub (right) 'the --> a' failed")

    OUTPUT_ = sub(qbf, _CK_"chicken", _CK_"rooster")
    call assert_delayed ( &
      OUTPUT_ == qbf, __LINE__ , &
      "character(4) sub no-op failed")

    allocate( PAGE_ , source = split(paths, _CK_":"))
    associate(msg => "character(4) path splitting test failed")
      call assert_delayed ( PAGE_ (1) == _CK_"/bin", __LINE__, msg)
      call assert_delayed ( PAGE_ (2) == _CK_"/sbin", __LINE__, msg)
      call assert_delayed ( PAGE_ (3) == _CK_"/usr/bin", __LINE__, msg)
      call assert_delayed ( PAGE_ (4) == _CK_"/usr/local/bin", __LINE__, msg)
    end associate

    deallocate( PAGE_ )
    allocate( PAGE_ , source = split(paths, _CK_""))
    do i = 1, size( PAGE_ )
      call assert_delayed ( PAGE_ (i) == paths, __LINE__ , &
        "character(4) no-op split on empty string test failed")
    end do

    deallocate( PAGE_ )
    allocate( PAGE_ , source = split(paths, _CK_";"))
    do i = 1, size( PAGE_ )
      call assert_delayed ( PAGE_ (i) == paths, __LINE__ , &
        "character(4) no-op split test failed")
    end do
    deallocate( PAGE_ )

    allocate( PAGE_ , source = split(_CK_"foo; bar; baz", _CK_"; "))
    call assert_delayed ( PAGE_ (1) == _CK_"foo", __LINE__ )
    call assert_delayed ( PAGE_ (2) == _CK_"bar", __LINE__ )
    call assert_delayed ( PAGE_ (3) == _CK_"baz", __LINE__ )

    call assert_delayed ( join( PAGE_ , _CK_"; ") == _CK_"foo; bar; baz", __LINE__ , &
      "character(4) rejoin using '; ' test of 'foo bar baz' failed")
    call assert_delayed ( join( PAGE_ , _CK_"")   == _CK_"foobarbaz",     __LINE__ , &
      "character(4) rejoin w/ zero length string 'foo bar baz' failed")

    call assert_delayed ( join( _CK_"A line" // nl, _CK_" ending") == _CK_"A line ending", __LINE__ , &
      "character(4) join scalar")

    call assert_delayed( "ascii," // _CK_" maybe ascii" == _CK_"ascii, maybe ascii", &
      __LINE__ , "character(1) concat character(4)")
    call assert_delayed( _CK_"one = " // 1.0_4 == _CK_"one = 1.00000", &
      __LINE__ , "character(4) concat real(4)")
    call assert_delayed( 1.0_4 // _CK_" = one" == _CK_"1.00000 = one", &
      __LINE__ , "real(4) concat character(4)")
    call assert_delayed( _CK_"one = " // 1.0_8 == _CK_"one = 1.00000", &
      __LINE__ , "character(4) concat real(8)")
    call assert_delayed( 1.0_8 // _CK_" = one" == _CK_"1.00000 = one", &
      __LINE__ , "real(8) concat character(4)")
    call assert_delayed( _CK_"one = " // 1.0_10 == _CK_"one = 1.00000", &
      __LINE__ , "character(4) concat real(10)")
    call assert_delayed( 1.0_10 // _CK_" = one" == _CK_"1.00000 = one", &
      __LINE__ , "real(10) concat character(4)")
    call assert_delayed( _CK_"one = " // 1.0_16 == _CK_"one = 1.00000", &
      __LINE__ , "character(4) concat real(16)")
    call assert_delayed( 1.0_16 // _CK_" = one" == _CK_"1.00000 = one", &
      __LINE__ , "real(16) concat character(4)")
    call assert_delayed( _CK_"one = " // 1_1 == _CK_"one = 1", &
      __LINE__ , "character(4) concat integer(1)")
    call assert_delayed( 1_1 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(1) concat character(4)")
    call assert_delayed( _CK_"one = " // 1_2 == _CK_"one = 1", &
      __LINE__ , "character(4) concat integer(2)")
    call assert_delayed( 1_2 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(2) concat character(4)")
    call assert_delayed( _CK_"one = " // 1_4 == _CK_"one = 1", &
      __LINE__ , "character(4) concat integer(4)")
    call assert_delayed( 1_4 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(4) concat character(4)")
    call assert_delayed( _CK_"one = " // 1_8 == _CK_"one = 1", &
      __LINE__ , "character(4) concat integer(8)")
    call assert_delayed( 1_8 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(8) concat character(4)")
    call assert_delayed( _CK_"one = " // 1_16 == _CK_"one = 1", &
      __LINE__ , "character(4) concat integer(16)")
    call assert_delayed( 1_16 // _CK_" = one" == _CK_"1 = one", &
      __LINE__ , "integer(16) concat character(4)")
    block
      logical(1) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(4) concat logical(1)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(1) concat character(4)")
    end block
    block
      logical(2) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(4) concat logical(2)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(2) concat character(4)")
    end block
    block
      logical(4) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(4) concat logical(4)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(4) concat character(4)")
    end block
    block
      logical(8) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(4) concat logical(8)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(8) concat character(4)")
    end block
    block
      logical(16) :: bool
      bool = .true.
      OUTPUT_ = _CK_".true. = " // bool
      call assert_delayed( OUTPUT_ == _CK_".true. = true", &
        __LINE__ , "character(4) concat logical(16)")
      OUTPUT_ = bool // _CK_" = .true."
      call assert_delayed( OUTPUT_ == _CK_"true = .true.", &
        __LINE__ , "logical(16) concat character(4)")
    end block

    associate( msg => "character(4) conversion to integer failed")
      call assert_delayed ( to_i( _CK_"-1" )  == -1 , __LINE__ , msg)
      call assert_delayed ( to_i( _CK_"1" )  == 1 , __LINE__ , msg)
      call assert_delayed ( to_i( _CK_"-99" )  == -99 , __LINE__ , msg)
      call assert_delayed ( to_i( _CK_"999" )  == 999 , __LINE__ , msg)
    end associate

    associate( msg => "character(4) conversion to logical failed")
      call assert_delayed ( .not. to_l( _CK_"false"   ), __LINE__ , msg)
      call assert_delayed (       to_l( _CK_"true"    ), __LINE__ , msg)
      call assert_delayed ( .not. to_l( _CK_".false." ), __LINE__ , msg)
      call assert_delayed (       to_l( _CK_".true."  ), __LINE__ , msg)
    end associate

    associate( msg => "character(4) conversion to real failed")
      call assert_delayed ( to_r( _CK_"0.0") == 0.0,  __LINE__ , msg)
      call assert_delayed ( to_r( _CK_"2.0") == 2.0,  __LINE__ , msg)
      call assert_delayed ( to_r( _CK_"-2.0") == -2.0,  __LINE__ , msg)
      call assert_delayed ( to_r( _CK_"2.0D0") == 2.0D0,  __LINE__ , msg)
      call assert_delayed ( to_r( _CK_"-2.0E0") == -2.0E0,  __LINE__ , msg)
    end associate

  end associate
#undef PAGE_
#undef _CK_
#undef OUTPUT_
#undef STRING_EXPECT_



#define TK 1
  associate( msg => "integer(1) to string conversion failed")
    call assert_delayed ( to_s(1_ TK) == "1", __LINE__ , msg)
    call assert_delayed ( to_s(-1_ TK) == "-1", __LINE__ , msg)
    call assert_delayed ( to_s(100_ TK) == "100", __LINE__ , msg)
    call assert_delayed ( to_s(-100_ TK) == "-100", __LINE__ , msg)
    call assert_delayed ( to_s(99_ TK) == "99", __LINE__ , msg)
    call assert_delayed ( to_s(-99_ TK) == "-99", __LINE__ , msg)
  end associate
#undef TK

#define TK 2
  associate( msg => "integer(2) to string conversion failed")
    call assert_delayed ( to_s(1_ TK) == "1", __LINE__ , msg)
    call assert_delayed ( to_s(-1_ TK) == "-1", __LINE__ , msg)
    call assert_delayed ( to_s(100_ TK) == "100", __LINE__ , msg)
    call assert_delayed ( to_s(-100_ TK) == "-100", __LINE__ , msg)
    call assert_delayed ( to_s(99_ TK) == "99", __LINE__ , msg)
    call assert_delayed ( to_s(-99_ TK) == "-99", __LINE__ , msg)
  end associate
#undef TK

#define TK 4
  associate( msg => "integer(4) to string conversion failed")
    call assert_delayed ( to_s(1_ TK) == "1", __LINE__ , msg)
    call assert_delayed ( to_s(-1_ TK) == "-1", __LINE__ , msg)
    call assert_delayed ( to_s(100_ TK) == "100", __LINE__ , msg)
    call assert_delayed ( to_s(-100_ TK) == "-100", __LINE__ , msg)
    call assert_delayed ( to_s(99_ TK) == "99", __LINE__ , msg)
    call assert_delayed ( to_s(-99_ TK) == "-99", __LINE__ , msg)
  end associate
#undef TK

#define TK 8
  associate( msg => "integer(8) to string conversion failed")
    call assert_delayed ( to_s(1_ TK) == "1", __LINE__ , msg)
    call assert_delayed ( to_s(-1_ TK) == "-1", __LINE__ , msg)
    call assert_delayed ( to_s(100_ TK) == "100", __LINE__ , msg)
    call assert_delayed ( to_s(-100_ TK) == "-100", __LINE__ , msg)
    call assert_delayed ( to_s(99_ TK) == "99", __LINE__ , msg)
    call assert_delayed ( to_s(-99_ TK) == "-99", __LINE__ , msg)
  end associate
#undef TK

#define TK 16
  associate( msg => "integer(16) to string conversion failed")
    call assert_delayed ( to_s(1_ TK) == "1", __LINE__ , msg)
    call assert_delayed ( to_s(-1_ TK) == "-1", __LINE__ , msg)
    call assert_delayed ( to_s(100_ TK) == "100", __LINE__ , msg)
    call assert_delayed ( to_s(-100_ TK) == "-100", __LINE__ , msg)
    call assert_delayed ( to_s(99_ TK) == "99", __LINE__ , msg)
    call assert_delayed ( to_s(-99_ TK) == "-99", __LINE__ , msg)
  end associate
#undef TK



  block
    logical(1) :: bool
    bool = .true.
    call assert_delayed( to_s(bool) == "true",  __LINE__ , &
      "logical(1) to string conversion failed" )
    call assert_delayed( bool , __LINE__ , &
      ".true. (logical(1) passed to assert_delayed)")
    bool = .false.
    call assert_delayed( to_s(bool) == "false", __LINE__ , &
      "logical(1) to string conversion failed")
    call assert_delayed( .not. bool , __LINE__ , &
      ".not. .false. (logical(1) passed to assert_delayed)")
  end block

  block
    logical(2) :: bool
    bool = .true.
    call assert_delayed( to_s(bool) == "true",  __LINE__ , &
      "logical(2) to string conversion failed" )
    call assert_delayed( bool , __LINE__ , &
      ".true. (logical(2) passed to assert_delayed)")
    bool = .false.
    call assert_delayed( to_s(bool) == "false", __LINE__ , &
      "logical(2) to string conversion failed")
    call assert_delayed( .not. bool , __LINE__ , &
      ".not. .false. (logical(2) passed to assert_delayed)")
  end block

  block
    logical(4) :: bool
    bool = .true.
    call assert_delayed( to_s(bool) == "true",  __LINE__ , &
      "logical(4) to string conversion failed" )
    call assert_delayed( bool , __LINE__ , &
      ".true. (logical(4) passed to assert_delayed)")
    bool = .false.
    call assert_delayed( to_s(bool) == "false", __LINE__ , &
      "logical(4) to string conversion failed")
    call assert_delayed( .not. bool , __LINE__ , &
      ".not. .false. (logical(4) passed to assert_delayed)")
  end block

  block
    logical(8) :: bool
    bool = .true.
    call assert_delayed( to_s(bool) == "true",  __LINE__ , &
      "logical(8) to string conversion failed" )
    call assert_delayed( bool , __LINE__ , &
      ".true. (logical(8) passed to assert_delayed)")
    bool = .false.
    call assert_delayed( to_s(bool) == "false", __LINE__ , &
      "logical(8) to string conversion failed")
    call assert_delayed( .not. bool , __LINE__ , &
      ".not. .false. (logical(8) passed to assert_delayed)")
  end block

  block
    logical(16) :: bool
    bool = .true.
    call assert_delayed( to_s(bool) == "true",  __LINE__ , &
      "logical(16) to string conversion failed" )
    call assert_delayed( bool , __LINE__ , &
      ".true. (logical(16) passed to assert_delayed)")
    bool = .false.
    call assert_delayed( to_s(bool) == "false", __LINE__ , &
      "logical(16) to string conversion failed")
    call assert_delayed( .not. bool , __LINE__ , &
      ".not. .false. (logical(16) passed to assert_delayed)")
  end block



  associate(msg => "unexpected value calling `to_s()` on real(4)")
    call assert_delayed ( to_s(0.0_4) == "0.00000", __LINE__, msg)
    call assert_delayed ( to_s(1.0_4) == "1.00000", __LINE__, msg)
    call assert_delayed ( to_s(-1.0_4) == "-1.00000", __LINE__, msg)
    call assert_delayed ( to_s(2.0_4) == "2.00000", __LINE__, msg)
    call assert_delayed ( to_s(-2.0_4) == "-2.00000", __LINE__, msg)
  end associate

  write(stdout,"(A)") ""
  write(stdout,"(A8,A8,A)") "real(4)", "   tiny", ": " //    tiny(1.0_4)
  write(stdout,"(A8,A8,A)") "real(4)", "epsilon", ": " // epsilon(1.0_4)
  write(stdout,"(A8,A8,A)") "real(4)", "   huge", ": " //    huge(1.0_4)
  write(stdout,"(A)") ""

  associate(msg => "unexpected value calling `to_s()` on real(8)")
    call assert_delayed ( to_s(0.0_8) == "0.00000", __LINE__, msg)
    call assert_delayed ( to_s(1.0_8) == "1.00000", __LINE__, msg)
    call assert_delayed ( to_s(-1.0_8) == "-1.00000", __LINE__, msg)
    call assert_delayed ( to_s(2.0_8) == "2.00000", __LINE__, msg)
    call assert_delayed ( to_s(-2.0_8) == "-2.00000", __LINE__, msg)
  end associate

  write(stdout,"(A)") ""
  write(stdout,"(A8,A8,A)") "real(8)", "   tiny", ": " //    tiny(1.0_8)
  write(stdout,"(A8,A8,A)") "real(8)", "epsilon", ": " // epsilon(1.0_8)
  write(stdout,"(A8,A8,A)") "real(8)", "   huge", ": " //    huge(1.0_8)
  write(stdout,"(A)") ""

  associate(msg => "unexpected value calling `to_s()` on real(10)")
    call assert_delayed ( to_s(0.0_10) == "0.00000", __LINE__, msg)
    call assert_delayed ( to_s(1.0_10) == "1.00000", __LINE__, msg)
    call assert_delayed ( to_s(-1.0_10) == "-1.00000", __LINE__, msg)
    call assert_delayed ( to_s(2.0_10) == "2.00000", __LINE__, msg)
    call assert_delayed ( to_s(-2.0_10) == "-2.00000", __LINE__, msg)
  end associate

  write(stdout,"(A)") ""
  write(stdout,"(A8,A8,A)") "real(10)", "   tiny", ": " //    tiny(1.0_10)
  write(stdout,"(A8,A8,A)") "real(10)", "epsilon", ": " // epsilon(1.0_10)
  write(stdout,"(A8,A8,A)") "real(10)", "   huge", ": " //    huge(1.0_10)
  write(stdout,"(A)") ""

  associate(msg => "unexpected value calling `to_s()` on real(16)")
    call assert_delayed ( to_s(0.0_16) == "0.00000", __LINE__, msg)
    call assert_delayed ( to_s(1.0_16) == "1.00000", __LINE__, msg)
    call assert_delayed ( to_s(-1.0_16) == "-1.00000", __LINE__, msg)
    call assert_delayed ( to_s(2.0_16) == "2.00000", __LINE__, msg)
    call assert_delayed ( to_s(-2.0_16) == "-2.00000", __LINE__, msg)
  end associate

  write(stdout,"(A)") ""
  write(stdout,"(A8,A8,A)") "real(16)", "   tiny", ": " //    tiny(1.0_16)
  write(stdout,"(A8,A8,A)") "real(16)", "epsilon", ": " // epsilon(1.0_16)
  write(stdout,"(A8,A8,A)") "real(16)", "   huge", ": " //    huge(1.0_16)
  write(stdout,"(A)") ""


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

  subroutine assert_delayed_l1(condition, line, message)
      logical(1), intent(in) :: condition
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
  subroutine assert_delayed_l2(condition, line, message)
      logical(2), intent(in) :: condition
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
  subroutine assert_delayed_l4(condition, line, message)
      logical(4), intent(in) :: condition
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
  subroutine assert_delayed_l8(condition, line, message)
      logical(8), intent(in) :: condition
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
  subroutine assert_delayed_l16(condition, line, message)
      logical(16), intent(in) :: condition
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
  subroutine string_expect_c1(input, expected)
    character(kind=1,len=*), intent(in) :: input, expected

    if( input /= expected ) then
      failures = failures + 1
      write(*,"(A)",ADVANCE="NO") "Expected output: "
      write(*,"(A)") expected
      write(*,"(A)",ADVANCE="NO") "Actual output:    "
      write(*,"(A)") input
    end if
  end subroutine
  subroutine string_expect_c4(input, expected)
    character(kind=4,len=*), intent(in) :: input, expected

    if( input /= expected ) then
      failures = failures + 1
      write(*,"(A)",ADVANCE="NO") "Expected output: "
      write(*,"(A)") expected
      write(*,"(A)",ADVANCE="NO") "Actual output:    "
      write(*,"(A)") input
    end if
  end subroutine
  end program
