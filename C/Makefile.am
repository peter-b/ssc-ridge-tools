bin_PROGRAMS = ridgetool ridgeconv show-tiff
noinst_LIBRARIES = libridgeutil.a libridgeio.a

CFLAGS = -Wall -pedantic $(GTK_CFLAGS)
LDFLAGS = -lm -ltiff $(GTK_LIBS)

libridgeutil_a_SOURCES = \
	ridgeutil.h \
	ridgeutil_surface.c \
	ridgeutil_multiproc.c

libridgeio_a_SOURCES = \
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
	filter.c \
	line.c \
	metrics.c \
	ridges.c \
	surface.c

ridgetool_LDADD = libridgeutil.a libridgeio.a

ridgeconv_SOURCES = \
	ridgeconv.h \
	ridgeconv.c \
	conv_csv.c \
	conv_svg.c \
	conv_tif.c

ridgeconv_LDADD = libridgeutil.a libridgeio.a

show_tiff_SOURCES = \
	show-tiff.c \
	surface.c \
	gui_surface.c

show_tiff_LDADD = libridgeutil.a

EXTRA_DIST = \
	gui_ridges.c
