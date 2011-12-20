#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([SSC Ridge Tools], [0.1], [p.brett@surrey.ac.uk])

AC_CONFIG_SRCDIR([ridgetool.h])
AC_CONFIG_AUX_DIR([build-tools])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADERS([config.h])

AM_INIT_AUTOMAKE([foreign])

# Checks for programs.
AC_PROG_CC
AC_PROG_CC_C99
AM_PROG_CC_C_O
AC_PROG_RANLIB
PKG_PROG_PKG_CONFIG

# Checks for libraries.
PKG_CHECK_MODULES([CAIRO], [cairo >= 1.10], ,
  AC_MSG_ERROR([Cairo 1.10.0 or later is required.]))
PKG_CHECK_MODULES([CAIRO_PNG], [cairo-png >= 1.10], ,
  AC_MSG_ERROR([Cairo PNG support 1.10.0 or later is required.]))
PKG_CHECK_MODULES([CAIRO_PDF], [cairo-pdf >= 1.10], ,
  AC_MSG_ERROR([Cairo PDF support 1.10.0 or later is required.]))
PKG_CHECK_MODULES([CAIRO_SVG], [cairo-svg >= 1.10], ,
  AC_MSG_ERROR([Cairo SVG support 1.10.0 or later is required.]))

PKG_CHECK_MODULES([GTK], [gtk+-2.0 >= 2.10.0], ,
  AC_MSG_ERROR([GTK+ 2.10.0 or later is required.]))
PKG_CHECK_MODULES([GSL], [gsl >= 1.15], ,
  AC_MSG_ERROR([GNU Scientific Library 1.15.0 or later is required.]))
AC_CHECK_LIB([m], [sqrt])
AC_CHECK_LIB([tiff], [TIFFOpen])

# Checks for header files.
AC_CHECK_HEADERS([stddef.h valgrind/memcheck.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_C_CONST
AC_HEADER_STDBOOL
AC_TYPE_INTPTR_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_UINT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPE([__int128_t],
              [AC_DEFINE([HAVE___INT128_T], [1],
                         [Define to 1 if the system has the type `__int128_t'.])],
              [], [])

# Check for pointer size
AC_CHECK_SIZEOF([intptr_t], [], [])

# Checks for library functions.
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_MMAP
AC_FUNC_REALLOC
AC_CHECK_FUNCS([memset munmap strchr strdup strerror])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
