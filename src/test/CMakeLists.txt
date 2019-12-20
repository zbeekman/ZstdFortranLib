# This file is added/included only when testing is enabled. Most testing logic happens automatically thanks to TargetCreator.cmake

# This extra test is to emulate being connected to a terminal on CI where no tty is connected by default.
foreach(file env_interface_test c_interfaces_test)
  add_test(NAME pty_${file} COMMAND $<TARGET_FILE:mock_pty> $<TARGET_FILE:${file}>)
endforeach()
