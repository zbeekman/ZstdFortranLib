add_library(c_env_interface OBJECT is_a_tty.c get_terminal_size.c get_connected_tty_lun.c)

if(CMAKE_HOST_SYSTEM_NAME STREQUAL Linux)
  find_library(LIBUTIL util-freebsd)
endif()

if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
  add_executable(mock_pty mock_pty.c)
  set_property(
    TARGET
    APPEND
    PROPERTY RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_BINDIR}/tests"
    )
endif()

if(${LIBUTIL})
  target_link_libraries(mock_pty PRIVATE ${LIBUTIL})
endif()
