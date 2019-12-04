add_subdirectory(c_interfaces)

add_fortran_lib(zstdfortran)
generate_and_add_sources(
  zstdfortran
  c_interfaces.F90
  env_interface.F90
  error_stack.F90
  strings.F90
  testing.F90
  testing_implementation.F90
  testing_interface.F90
  # error_types.F90
  )
target_sources(zstdfortran PUBLIC $<TARGET_OBJECTS:FACE> PUBLIC $<TARGET_OBJECTS:c_env_interface>)

# Should do something better/more clever here
target_link_libraries(zstdfortran PRIVATE FACE)

# This test intentionally converts strings to reals and does equality comparisons for known binary representations, e.g., 0.0, 2.0
map_src_to_generated(GENERATED_SRC test/strings_test.F90)
set_source_files_properties(${GENERATED_SRC} PROPERTIES COMPILE_OPTIONS -Wno-compare-reals)

add_targets_tests(zstdfortran)

# Allow dedicated CMake Lists.txt for some finer control, if present
conditionally_add_test_dir()
