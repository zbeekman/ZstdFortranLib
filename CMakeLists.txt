cmake_minimum_required(VERSION 3.13.4)
cmake_policy(VERSION 3.13.4...3.15.5)

# Make project modules available
list(INSERT CMAKE_MODULE_PATH 0 "${CMAKE_CURRENT_LIST_DIR}/cmake/modules")

# Disallow in source builds
include(CheckOutOfSourceBuild)
check_out_of_source_build()

# Get information from git, if it repository
include(VersionHelper)
get_vcs_info()
message(STATUS "GIT_COMMIT: ${GIT_FULL_DESCRIBE}")

# Set default configuration types
include(SetConfigTypes)
set_config_types(DEFAULT_DEV_CONFIG RelWithDebInfo CONFIGS RuntimeDebug CodeCoverage)

# Use relative install rpaths and don't use build rpaths: Setup build directory to match install directory
include(RelocatableReproducibleHelper)

# Set some default compiler flags per compiler/language
set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/cmake/config_defaults.cmake)

project(
  ZstdFortranLib
  VERSION 0.0.1
  DESCRIPTION "Zaak's (missing) Standard Fortran Library"
  HOMEPAGE_URL "https://izaakbeekman.com"
  LANGUAGES Fortran C
  )

if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
  include(CTest)
endif()

# Jinja2 based templating system
include(Jin2For)
# Add function to create/add Fortran libraries and automatically find/enable tests
include(TargetCreator)

# Add external projects
include(${CMAKE_CURRENT_LIST_DIR}/extern/CMakeLists.txt)

add_subdirectory(src)

include(UninstallTarget)
