bin_PROGRAMS = ridgetool ridgeconv ridgecull xridgeclass ridgeclassexport \
	 ridgeclassstat
lib_LTLIBRARIES = libridgetool.la
include_HEADERS = ridgeutil.h ridgeio.h
dist_man_MANS = ridgetool.1 ridgeconv.1 ridgecull.1 ridgeclassexport.1 \
	ridgeclassstat.1 xridgeclass.1

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libridgetool.pc

CFLAGS = -g -Wall -pedantic $(GSL_CFLAGS) $(GTK_CFLAGS)
LDFLAGS = -lm -ltiff $(GSL_LIBS) $(GTK_LIBS)
LDADD = libridgetool.la

libridgetool_la_LDFLAGS = -version-info 1:0:0
libridgetool_la_SOURCES = \
	ridgeutil.h \
	ridgeutil_filter.c \
	ridgeutil_scalespace.c \
	ridgeutil_surface.c \
	ridgeutil_tiffio.c \
	ridgeutil_multiproc.c \
	ridgeio.h \
	ridgeio_array.c \
	ridgeio_data.c \
	ridgeio_line.c \
	ridgeio_lowlvl.c \
	ridgeio_point.c \
	ridgeio_segment.c

ridgetool_SOURCES = \
	ridgetool.h \
	ridgetool.c \
	eigen.c \
	export.c \
	line.c \
	metrics.c \
	ridges.c

ridgeconv_SOURCES = \
	ridgeconv.h \
	ridgeconv.c \
	conv_csv.c \
	conv_mask.c \
	conv_svg.c

ridgeconv_CFLAGS = $(CAIRO_CFLAGS) $(CAIRO_PNG_CFLAGS) \
	$(CAIRO_PDF_CFLAGS) $(CAIRO_SVG_CFLAGS)
ridgeconv_LDFLAGS = $(CAIRO_LIBS) $(CAIRO_PNG_LIBS) \
	$(CAIRO_PDF_LIBS) $(CAIRO_SVG_LIBS)

ridgecull_SOURCES = \
	ridgecull.c

xridgeclass_SOURCES = \
	xridgeclass.c

ridgeclassexport_SOURCES = \
	ridgeclassexport.c

ridgeclassstat_SOURCES = \
	ridgeclassstat.c

ACLOCAL_AMFLAGS = -I m4
libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status libtool

DISTCLEANFILES = libridgetool.pc
