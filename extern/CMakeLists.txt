include(QueryFortranStringTypes)

# #######################
# FACE
# #######################

foreach(combo ${ascii_supported} ${ascii_neq_default} ${ucs4_supported})
  if(${combo})
    list(APPEND FACE_DEFS ${combo})
  endif()
endforeach()

message(STATUS "Configuring FACE external library.")
message(STATUS "FACE compile definitions: ${FACE_DEFS}")
add_library(FACE OBJECT ${CMAKE_CURRENT_LIST_DIR}/FACE/face.F90)
target_compile_definitions(FACE PUBLIC ${FACE_DEFS})

# #########################
# Functional-Fortran
# #########################

message(STATUS "Configuring Functional-Fortran external library.")
foreach(src functional interfaces)
  list(APPEND ff_sources "${CMAKE_CURRENT_LIST_DIR}/functional-fortran/src/lib/mod_${src}.f90")
endforeach()
add_library(functional-fortran OBJECT ${ff_sources})
