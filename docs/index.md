---
title: Z' Standard Fortran Library
summary: High level documentation for ZstdFortranLib
authors:
    - Izaak Beekman
---

# Welcome

This is where the high-level documentation for Z' Standard Fortran Library is hosted.

??? note "Table of contents"
    [TOC]

# Capabilities and goals

* A reusable, opinionated & complete library to interface with the system & perform common tasks
* Tests are as quick and easy as possible to run and add (via some [CMake] black magic)
* Maintain a high test coverage, documented with [Codecov]
* Documented user and developer APIs with [FORD]
* Vendor and package popular, useful libraries from the greater Fortran community
* Use generic programming & templating (via [Jin2For]) to reduce the size and repetition of source code
* Provide the following features:
    * Testing classes, functions and syntactic sugar
    * Deferred exception handling through an error stack object with stack traces
    * Deferred, immediate and debug-only runtime assertions
    * DRY and DWIM capabilities, especially when working with strings
    * String functions, classes and syntactic sugar
    * File manipulation functions, classes and syntactic sugar
    * OS/Environment interfacing functions, classes and syntactic sugar

*[DRY]: Don't Repeat Yourself
*[DWIM]: Do What I Mean

[CMake]: https://cmake.org
[Codecov]: https://codecov.io
[FORD]: https://github.com/Fortran-FOSS-Programmers/FORD
[Jin2For]: https://pypi.org/project/jin2for/

# Why that name?

`ZstdFortranLib` is from both:

* "Zaak's Standard Fortran Library" or "Z' Standard Fortran Library" for short
* "The Standard Fortran Library", said with a precise sounding (and possibly German) accent

# Prerequisites

Currently, Python3 and the [Jin2For] template engine are required to build `ZstdFortranLib`.
At a later date, the build system will be adjusted to use the output of the processed templates
when [Jin2For] and/or Python3 can't be found or are not available. This will be helpful for
package maintainers and users who do not wish to develop or extend `ZstdFortranLib`, but only
build and link against it. At that time, the required dependencies will be reduced to a
suitable Fortran and C compiler, as well as a new-ish version of CMake.

## Developer prerequisites

At this time the (developoment/developer) dependencies are:

* A Unix-like operating system
    * Tested on macOS and Linux (Ubuntu)
* A Fortran compiler supporting certain Fortran 2008 & 2018 features
    * Testing is currently done with GFortran 7, 8 and 9
* A C compiler
    * GCC 7, 8 and 9 tested, Apple Clang works too
* A new-ish Python3 interpreter >= 3.6
* [Pipenv]
    * `pip3 install --upgrade --user pipenv`
    * Make sure your [Python USER_BASE] `bin` directory is in your `$PATH`
        * `export "PATH=$(python3 -m site --user-base)/bin:$PATH"`
    * Management of infrastructure, virtual environments

On macOS with [Homebrew] (or Linux when using Homebrew for linux), all of these can be installed with:

``` bash
brew install gcc@9 python pipenv
```

[Jin2For]: https://pypi.org/project/jin2for/
[Pipenv]: https://pipenv.kennethreitz.org/en/latest/
[Python USER_BASE]: https://docs.python.org/3/library/site.html#site.USER_BASE
[Homebrew]: https://brew.sh

## Installing remaining tools with [Pipenv]

[Pipenv] is used to improve reproducibility builds and environments and to manage
the installation of software and tooling specific to this project. This way you don't
need root, and won't change your global or user-specific development environment.
To finish setting up the development environment and pre-requisites follow these steps:

1. `pipenv sync --dev`: Install [CMake], [Jin2For] and other tools to build documentation
2. `pipenv run pre-commit install --install-hooks`: Install developer pre-commit hooks (optional for users)

# Building `ZstdFortranLib`

The build system for `ZstdFortranLib` is a normal [CMake] based build system. However, at this time
the build system is focused on contributors and developers. Until the non-developer, user focused
build system is finished, as discussed [above](#prerequisites), everyone
must follow the developer build instructions.

For both developer builds and user builds, the appropriate compilers should be specified with environment variables:

1. `export FC=$(brew --prefix gcc@9)/bin/gfortran-9`: Set the Fortran compiler to use (specify correct path if not using [Homebrew])
2. `export FC=$(brew --prefix gcc@9)/bin/gcc-9`: Set the C compiler to use (specify correct path if not using [Homebrew])


## Developer build instructions

There are two possible techniques for configuring, building and testing the library at this time.
The first approach is to spawn a virtual environment by running `pipenv shell` and execute
build commands manually as is typical for canonical CMake projects. This workflow is best for
experienced users, those developing the library and developers writing documentation for the library.
Once in the virtual environment the process is effectively identical to the user-focussed build process, detailed
[below](#user-build-instructions).

The second approach is to use the convenience scripts packaged in the Pipfile, and invoked with
`pipenv run <action>`. Here are the various actions currently available:

* `pipenv run nuke_build`
    * Delete the build directory including all compiled objects and the [CMake] cache
* `pipenv run configure`
    * Create a `build` directory if needed, and configure the project with [CMake]
* `pipenv run gather_logs`
    * Get [CMake] logs and put them in one place to help with debugging
* `pipenv run build`
    * Build (compile) the library in parallel, keep going on errors
* `pipenv run verbose_build`
    * Turn on verbose output and build in serial but keep going on errors
* `pipenv run unit_test`
    * Run all tests using [CTest] with the unit test label
* `pipenv run integration_test`
    * Run all tests using [CTest] without the unit test label

## User build instructions

After entering the virtual environment with `pipenv shell` or when performing a user-build
(once this is enabled, see [above](#prerequisites)) the library compilation and installation
follows the normal, canonical [CMake] process. This is outlined below.

``` bash
# FC and CC should already be set
mkdir build
cd build
# Next, configure with CMake:
cmake -DCMAKE_INSTALL_PREFIX:PATH=</path/to/desired/install/location> ..
# Compile in parallel, but emit verbose info if an error occurs:
make -j <N_CPUS> -k || make VERBOSE=1 -k
# Optional: install library; might require root/sudo if install location requires it
make -j <N_CPUS> install
# Test with ctest, use -L to specify a label or -LE to exclude a label
ctest --output-on-failure [-L unit]
# If you installed and want to remove the installation
make uninstall
```
