include_guard(DIRECTORY)

# Get Fortran compiler major version number
string(REGEX REPLACE [[([0-9]+)(\.[0-9]+)*]] [[\1]] Fortran_COMPILER_MAJ_VERSION ${CMAKE_Fortran_COMPILER_VERSION})

set(FCIDwV ${CMAKE_Fortran_COMPILER_ID}-${Fortran_COMPILER_MAJ_VERSION})

# Set module installation location
set(MODULE_INSTALL_DIR ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${FCIDwV})

# Set default module output location
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/${MODULE_INSTALL_DIR})

install(DIRECTORY ${PROJECT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  COMPONENT ${PROJECT_NAME}_Development
  FILES_MATCHING
  PATTERN "*.mod"
  PATTERN "*.smod"
  )

# Function to add targets and setup tests in a sane way for Fortran
function(add_Fortran_lib lib_name)

  add_library(${lib_name}
    ${ARGN})
  target_include_directories(${lib_name}
    PUBLIC $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
    INTERFACE $<INSTALL_INTERFACE:${MODULE_INSTALL_DIR}>)
  set_target_properties(${lib_name}
    PROPERTIES
    LINKER_LANGUAGE Fortran)
  add_library(${PROJECT_NAME}::${lib_name} ALIAS ${lib_name})

  set_target_properties(${lib_name} PROPERTIES
    VERSION
    ${PROJECT_VERSION}
    SOVERSION
    ${PROJECT_VERSION_MAJOR}
    )
  # Logic for installation and target export, etc.
  install(TARGETS ${lib_name} EXPORT  ${PROJECT_NAME}-targets
    ARCHIVE
    DESTINATION ${CMAKE_INSTALL_LIBDIR}
    COMPONENT ${PROJECT_NAME}_Development
    LIBRARY
    DESTINATION ${CMAKE_INSTALL_LIBDIR}
    COMPONENT ${PROJECT_NAME}_Runtime
    NAMELINK_COMPONENT ${PROJECT_NAME}_Developement
    RUNTIME
    DESTINATION ${CMAKE_INSTALL_BINDIR}
    COMPONENT ${PROJECT_NAME}_Runtime
    INCLUDES
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}"
    COMPONENT ${PROJECT_NAME}_Development
    )
endfunction()

function(map_src_to_generated GENERATED INPUT)
  # Generate sources from the templates directory
  set(OUTPUT_DIR ${PROJECT_SOURCE_DIR}/generated/${CMAKE_Fortran_COMPILER_ID})
  if(NOT EXISTS ${OUTPUT_DIR})
    message(STATUS "Creating generated/${CMAKE_Fortran_COMPILER_ID} directory in source tree.")
    file(MAKE_DIRECTORY ${OUTPUT_DIR})
  endif()
  get_filename_component(ABS_SRC ${INPUT} ABSOLUTE)
  file(RELATIVE_PATH GENERATED_SOURCE ${PROJECT_SOURCE_DIR}/src ${ABS_SRC})
  get_filename_component(GEN_DIR ${GENERATED_SOURCE} DIRECTORY)
  get_filename_component(GEN_SRC_NAME ${GENERATED_SOURCE} NAME)
  if(GEN_DIR)
    if(NOT EXISTS ${OUTPUT_DIR}/${GEN_DIR})
      file(MAKE_DIRECTORY ${OUTPUT_DIR}/${GEN_DIR})
    endif()
    set(${GENERATED} ${OUTPUT_DIR}/${GEN_DIR}/${GEN_SRC_NAME} PARENT_SCOPE)
  else()
    set(${GENERATED} ${OUTPUT_DIR}/${GEN_SRC_NAME} PARENT_SCOPE)
  endif()
endfunction()

function(generate_and_add_sources target)
  foreach(source ${ARGN})
    get_filename_component(ABS_SRC ${source} ABSOLUTE)
    map_src_to_generated(GENERATED_SOURCE ${ABS_SRC})
    message(STATUS "Generating ${GENERATED_SOURCE} from ${source}")
    jin2for(${GENERATED_SOURCE}
      ${ABS_SRC})
    target_sources(${target}
      PRIVATE ${GENERATED_SOURCE})
  endforeach()
endfunction()

function(default_test_properties)
  set(vars_to_set
    CMAKE_SYSTEM=${CMAKE_SYSTEM}
    CMAKE_SYSTEM_PROCESSOR=${CMAKE_SYSTEM_PROCESSOR})
  if(LINUX)
    list(APPEND vars_to_set CMAKE_LIBRARY_ARCHITECTURE=${CMAKE_LIBRARY_ARCHITECTURE})
  endif()
  set_tests_properties(${ARGN}
    PROPERTIES
    FAIL_REGULAR_EXPRESSION "[Tt]est [Ff]ailed;TEST FAILED"
    ENVIRONMENT "${vars_to_set}")
endfunction()

function(add_targets_tests target)
  # Add tests corresponding to targets and their source files
  if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
    # Don't put test executables in with executables to distribute
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/tests)
    # Don't put test module files in with the mod files to be installed
    set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
    # Only add and build tests if testing is enabled
    if(DEFINED GIT_COMMIT_HASH)
      # Developer build, look for new files and re-run cmake if they appear
      file(GLOB CANDIDATE_TARGET_TESTS
        LIST_DIRECTORIES false RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} CONFIGURE_DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/${target}_test.[cfF]9?0?")
      file(GLOB CANDIDATE_UNIT_TESTS
        LIST_DIRECTORIES false RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/test CONFIGURE_DEPENDS
        "${CMAKE_CURRENT_SOURCE_DIR}/${target}test/*_test.[cfF]9?0?")
    else()
      file(GLOB CANDIDATE_TARGET_TESTS
        LIST_DIRECTORIES false RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
        "${CMAKE_CURRENT_SOURCE_DIR}/${target}_test.[cfF]9?0?")
    endif()
    # If there are multiple tests with different file-extensions, proceed but warn the user
    list(LENGTH CANDIDATE_TARGET_TESTS N_TEST_FILES)
    if(N_TEST_FILES GREATER 1)
      message(WARNING "Multiple tests found for ${target}: ${CANDIDATE_TARGET_TESTS}")
    endif()
    message(STATUS "Candidate target tests for ${target}: ${CANDIDATE_TARGET_TESTS}")
    get_filename_component(target_name_wle ${target} NAME_WLE)
    # Do the test(s) corresponding to the target that is in the current directory
    foreach(test IN LISTS CANDIDATE_TARGET_TESTS)
      if(N_TEST_FILES EQUAL 1)
        get_filename_component(test_name ${test} NAME_WLE)
      else()
        message(WARNING "Found to many candidate tests for target: ${target}")
      endif()
      if(${target_name_wle}_test STREQUAL ${test_name})
        message(STATUS "Adding executable: ${test_name}")
        add_executable(${test_name}
          ${test})
        target_link_libraries(${test_name}
          PRIVATE ${target})
        message(STATUS "Adding test: ${test_name}")
        add_test(NAME ${test_name}
          COMMAND $<TARGET_FILE:${test_name}>)
        set_tests_properties(${test_name}
          PROPERTIES
          LABELS ${target})
        default_tests_properties(${test_name})
      endif()
    endforeach()

    # Now get the list of source files corresponding to the target
    get_target_property(ALL_SOURCES ${target} SOURCES)
    string(GENEX_STRIP "${ALL_SOURCES}" SOURCES)
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test)
      # Keep the module files from the test subdirectory in the subdirectory
      set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/test)
      message(STATUS "Searching for tests corresponding to ${target}'s sources: ${SOURCES}")
      foreach(source IN LISTS SOURCES)
        get_filename_component(src_name ${source} NAME_WLE)
        message(STATUS "Finding tests for ${src_name}")
        foreach(ext F90 f90 c)
          set(candidate ${CMAKE_CURRENT_SOURCE_DIR}/test/${src_name}_test.${ext})
          if(EXISTS ${candidate})
            message(STATUS "Adding executable: ${src_name}_test")
            if(ext STREQUAL F90)
              map_src_to_generated(GENERATED_SOURCE ${candidate})
              jin2for(${GENERATED_SOURCE} ${candidate})
              set(test_src ${GENERATED_SOURCE})
            else()
              set(test_src ${candidate})
            endif()
            add_executable(${src_name}_test ${test_src})
            target_include_directories(${src_name}_test
              PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
            target_link_libraries(${src_name}_test
              PRIVATE ${target})
            message(STATUS "Adding test: ${src_name}_test")
            add_test(NAME ${src_name}_test
              COMMAND $<TARGET_FILE:${src_name}_test>)
            set_tests_properties(${src_name}_test
              PROPERTIES
              LABELS "unit;${target}")
            default_test_properties(${src_name}_test)
            break() # Needed for case-insensitive OSes!!!!
          endif()
        endforeach()
      endforeach()
    else()
      message(STATUS "No test directory in ${CMAKE_CURRENT_SOURCE_DIR}")
    endif()
  endif()
endfunction()

function(conditionally_add_test_dir)
  # Add tests corresponding to targets and their source files
  if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME AND BUILD_TESTING)
    if(DEFINED GIT_COMMIT_HASH)
      # Developer build, keep an eye out for new CMakeLists.txt
      file(GLOB test_dir_CMAKE_LISTS
        LIST_DIRECTORIES FALSE CONFIGURE_DEPENDS
        "test/CMakeLists.txt")
    endif()
    # Allow dedicated CMake Lists.txt for some finer control, if present
    if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/test/CMakeLists.txt)
      add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/test)
    endif()
  endif()
endfunction()
