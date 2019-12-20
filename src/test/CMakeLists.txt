# This file is added/included only when testing is enabled. Most testing logic happens automatically thanks to TargetCreator.cmake

# This extra test is to emulate being connected to a terminal on CI where no tty is connected by default.
if(NOT CMAKE_HOST_SYSTEM_NAME STREQUAL Linux)
  foreach(file env_interface_test c_interfaces_test)
    add_test(NAME pty_${file} COMMAND $<TARGET_FILE:mock_pty> $<TARGET_FILE:${file}>)
    set_tests_properties(pty_${file} PROPERTIES LABELS "unit;zstdfortran}")
  endforeach()
endif()
