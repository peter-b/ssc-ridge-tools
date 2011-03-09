bin_PROGRAMS = ridgetool show-tiff qr-symmv
noinst_LIBRARIES = libridgeio.a

CFLAGS = -Wall -pedantic $(GTK_CFLAGS)
LDFLAGS = -lm -ltiff $(GTK_LIBS)

libridgeio_a_SOURCES = \
	ridgeio.h \
	ridgeio_data.c \
	ridgeio_line.c \
	ridgeio_lowlvl.c \
	ridgeio_point.c \
	ridgeio_segment.c

ridgetool_SOURCES = \
	ridgetool.c \
	eigen.c \
	filter.c \
	line.c \
	metrics.c \
	multiproc.c \
	ridges.c \
	surface.c \
	gui_surface.c \
	gui_ridges.c

qr_symmv_SOURCES = \
	qr-symmv.c \
	eigen.c

show_tiff_SOURCES = \
	multiproc.c \
	show-tiff.c \
	surface.c \
	gui_surface.c
