include(QueryFortranStringTypes)

foreach(combo ${ascii_supported} ${ascii_neq_default} ${ucs4_supported})
  if(${combo})
    list(APPEND FACE_DEFS ${combo})
  endif()
endforeach()

message(STATUS "FACE compile definitions: ${FACE_DEFS}")
add_library(FACE OBJECT ${CMAKE_CURRENT_LIST_DIR}/face.F90)
target_compile_definitions(FACE PUBLIC ${FACE_DEFS})
