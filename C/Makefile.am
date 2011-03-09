bin_PROGRAMS = ridgetool show-tiff
noinst_LIBRARIES = libridgeio.a

CFLAGS = -Wall -pedantic $(GTK_CFLAGS)
LDFLAGS = -lm -ltiff $(GTK_LIBS)

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
	multiproc.c \
	ridges.c \
	surface.c

ridgetool_LDADD = libridgeio.a

show_tiff_SOURCES = \
	multiproc.c \
	show-tiff.c \
	surface.c \
	gui_surface.c

EXTRA_DIST = \
	gui_ridges.c
