include_guard(DIRECTORY)

# set variables used for compile definitions of targets after support check
include(CheckFortranSourceRuns)

# Set defaults
set(ascii_supported FALSE)
set(ascii_neq_default FALSE)
set(ucs4_supported FALSE)

check_fortran_source_runs(
  "program ascii_support;
         integer, parameter :: ascii = selected_char_kind('ascii');
         if(ascii < 0) stop 1;
     end program ascii_support"
  ASCII_SUPPORTED
  SRC_EXT
  f90
  )
if(ASCII_SUPPORTED)
  set(ascii_supported "ASCII_SUPPORTED")
endif()

check_fortran_source_runs(
  "program ascii_neq_default;
         integer, parameter :: ascii = selected_char_kind('ascii');
         integer, parameter :: default = selected_char_kind('default');
         if(ascii == default) stop 1;
     end program ascii_neq_default"
  ASCII_NEQ_DEFAULT
  SRC_EXT
  f90
  )
if(ASCII_NEQ_DEFAULT)
  set(ascii_neq_default "ASCII_NEQ_DEFAULT")
endif()

check_fortran_source_runs(
  "program ucs4_support;
         integer, parameter :: ucs4 = selected_char_kind('iso_10646');
         if(ucs4 < 0) stop 1;
     end program ucs4_support"
  UCS4_SUPPORTED
  SRC_EXT
  f90
  )
if(UCS4_SUPPORTED)
  set(ucs4_supported "UCS4_SUPPORTED")
endif()
