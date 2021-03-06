#                     The MCLinker project
#
# This file is distributed under the University of Illinois Open Source
# License. See LICENSE.TXT for details.

##############################################################################
### WARNING: this file contains embedded tabs.  Do not run untabify on this file.

####################
#  m4 includes
m4_include(m4/acx_pthread.m4)
m4_include(m4/link_options.m4)
####################
#  Autoconf/Automake Initialization

dnl AC_INIT(PKG_NAME, BUILD_VERSION, BUG_REPORTER)
AC_INIT([MCLinker],[dev],[mclinker@googlegroups.com])
AC_PREREQ(2.59)

AC_CONFIG_SRCDIR([./COPYING])
AC_CONFIG_AUX_DIR([scripts])
AC_CONFIG_MACRO_DIR([m4])

dnl Check the target for which we're linking and the host that will do the
dnl linking. This will tell us which MCLINKER will be used for linking
dnl input objects into output. This needs to be done early because
dnl following tests depend on it.
AC_CANONICAL_TARGET

AM_INIT_AUTOMAKE([1.9 dist-bzip2 foreign subdir-objects silent-rules])

AM_SILENT_RULES([yes])

####################
#  Versioning, see VERSION
MAJOR=2
MINOR=9
MICRO=0
BUILD=dev

DEV_VERSION=$MAJOR.$MINOR.$MICRO-$BUILD
PKG_VERSION=$MAJOR.$MINOR.$MICRO-$BUILD
MCLD_VERSION=$DEV_VERSION
AC_SUBST(MCLD_VERSION)
AC_DEFINE_UNQUOTED([MCLD_VERSION], ["$MCLD_VERSION"])
AH_TEMPLATE([MCLD_VERSION], [MCLINKER version])

AC_PROG_CC
AC_PROG_CXX

####################
#  --enable-shrink-binary-size : check whether they want to shrink mclinker size:
AC_ARG_ENABLE(shrink-binary-size, AS_HELP_STRING(
 --enable-shrink-binary-size,[Shrink mclinker size (default is NO)]),,enableval="no")

if test ${enableval} = "no" ; then
  AC_SUBST(ENABLE_SHRINK_BINARY_SIZE,[0])
else
  AC_SUBST(ENABLE_SHRINK_BINARY_SIZE,[1])
fi

if test "$ENABLE_SHRINK_BINARY_SIZE" = "1"; then
    ####################
    #  Compiler flags for binary size reduction
    AC_MSG_CHECKING([binary size reduction compiler flags])
    CXX_FLAG_CHECK(DATA_SECTIONS, [-fdata-sections])
    CXX_FLAG_CHECK(FUNCTION_SECTIONS, [-ffunction-sections])
    CXX_FLAG_CHECK(VISIBILITY_INLINES_HIDDEN, [-fvisibility-inlines-hidden])
    CFLAGS="-Os $CFLAGS $DATA_SECTIONS $FUNCTION_SECTIONS $VISIBILITY_INLINES_HIDDEN"
    CXXFLAGS="-Os $CXXFLAGS $DATA_SECTIONS $FUNCTION_SECTIONS $VISIBILITY_INLINES_HIDDEN"

    ####################
    #  Linker flags for binary size reduction
    #  (cooperative with -ffunction-sections, -fdata-sections)
    AC_MSG_CHECKING([binary size reduction linker flags])
    AC_LINK_USE_GC_SECTIONS([GC_SECTIONS])
    LDFLAGS="$LDFLAGS $GC_SECTIONS"
fi

AC_PATH_PROG([FLEX],[flex])
AX_PROG_FLEX_VERSION([2.5.35],[],[AC_MSG_ERROR([Require flex 2.5.35 or later])])
AC_PATH_PROG([BISON],[bison])
AX_PROG_BISON_VERSION([2.4.3],[3.1],[],
                      [AC_MSG_ERROR([Require bison >= 2.4.3 and < 3.1])])

AC_LANG([C++])
LT_INIT

####################
#  Check for libraries

#  Configure pthreads.
AC_ARG_WITH([pthreads],
            [AS_HELP_STRING([--with-pthreads],
               [use pthreads (default is yes)])],
            [with_pthreads=$withval],
            [with_pthreads=check])

have_pthreads=no
AS_IF([test "x$with_pthreads" != "xno"],
      [ACX_PTHREAD(
        [],
        [AS_IF([test "x$with_pthreads" != "xcheck"],
               [AC_MSG_FAILURE(
                 [--with-pthreads was specified, but unable to be used])])])
       have_pthreads="$acx_pthread_ok"])
AM_CONDITIONAL([HAVE_PTHREADS],[test "x$have_pthreads" == "xyes"])
AC_SUBST(PTHREAD_CFLAGS)
AC_SUBST(PTHREAD_LIBS)

####################
# Configure optimized build
AC_ARG_ENABLE(optimized,
            [AS_HELP_STRING([--enable-optimized],
                [turn on unit-tests (default is no)])],
            [case "${enableval}" in
              yes) optimized=true ;;
              no)  optimized=false ;;
              *) AC_MSG_ERROR(bad value ${enableval} for --enable-optimized) ;;
             esac],
             [optimized=false])
AM_CONDITIONAL([ENABLE_OPTIMIZED],[test "x${optimized}" == "xtrue" ])

####################
# Configure compiler warnings
AC_MSG_CHECKING([for optional compiler flags])
CXX_FLAG_CHECK(NO_VARIADIC_MACROS, [-Wno-variadic-macros])
CXX_FLAG_CHECK(NO_COVERED_SWITCH_DEFAULT, [-Wno-covered-switch-default])
CXX_FLAG_CHECK(NO_C99_EXTENSIONS, [-Wno-c99-extensions])
AC_MSG_RESULT([$NO_VARIADIC_MACROS $NO_COVERED_SWITCH_DEFAULT $NO_C99_EXTENSIONS])

####################
# Check for header files
AC_CHECK_HEADERS([fcntl.h unistd.h])
AC_CHECK_HEADERS([sys/mman.h])
AC_CHECK_HEADERS([cxxabi.h])
AC_CHECK_HEADERS([zlib.h])

####################
# Configure LLVM
CHECK_LLVM([3.5])
ENUM_MCLD_TARGETS
CHECK_ALONE_TARGET

# Config.h
AC_CONFIG_HEADER(include/mcld/Config/Config.h)

AH_TOP([
//===- Config.h.in --------------------------------------------------------===//
//
//                     The MCLinker Project
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef MCLD_CONFIG_H
#define MCLD_CONFIG_H
])

AH_BOTTOM([
#define MCLD_REGION_CHUNK_SIZE 32
#define MCLD_NUM_OF_INPUTS 32
#define MCLD_SECTIONS_PER_INPUT 16
#define MCLD_SYMBOLS_PER_INPUT 128
#define MCLD_RELOCATIONS_PER_INPUT 1024

#define MCLD_SEGMENTS_PER_OUTPUT 8

#endif
])

####################
# OUTPUT
AC_CONFIG_FILES([VERSION])
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([include/Makefile])
AC_CONFIG_FILES([lib/Makefile])
AC_CONFIG_FILES([utils/Makefile])
AC_CONFIG_FILES([utils/gtest/Makefile])
AC_CONFIG_FILES([utils/gtestmain/Makefile])
AC_CONFIG_FILES([utils/zlib/Makefile])
AC_CONFIG_FILES([unittests/Makefile])
AC_CONFIG_FILES([include/mcld/Config/Targets.def])
AC_CONFIG_FILES([include/mcld/Config/Linkers.def])
AC_CONFIG_FILES([tools/Makefile])
AC_CONFIG_FILES([tools/mcld/Makefile])
AC_CONFIG_FILES([test/Makefile])

AC_OUTPUT
