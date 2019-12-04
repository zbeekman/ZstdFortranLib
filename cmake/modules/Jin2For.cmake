include_guard(GLOBAL)

find_package(Python COMPONENTS Interpreter)

function(jin2for generated template)
  message(STATUS "Adding jin2for generated target: ${generated}")
  get_filename_component(FILE_EXT ${generated} LAST_EXT)
  get_filename_component(OUTDIR ${generated} DIRECTORY)
  get_filename_component(OUTBASENAME ${generated} NAME_WLE)
  get_filename_component(INBASENAME ${template} NAME_WLE)
  if(NOT "${OUTBASENAME}" STREQUAL "${INBASENAME}")
    message(FATAL_ERROR "Cannot generate a file with a different basename using jin2for!\n
       ${template} --> ${generated}"
            )
  endif()
  add_custom_command(
    OUTPUT ${generated}
    COMMAND
      jin2for -g ${CMAKE_Fortran_COMPILER} -e ${FILE_EXT} -O ${OUTDIR} ${template} ${ARGN}
    MAIN_DEPENDENCY ${template}
    DEPENDS ${template} ${ARGN}
    COMMENT "Generating ${generated}"
    )
endfunction()
