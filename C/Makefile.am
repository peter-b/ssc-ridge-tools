bin_PROGRAMS = ridgetool show-tiff qr-symmv

CFLAGS = -Wall -pedantic $(GTK_CFLAGS)
LDFLAGS = -lm -ltiff $(GTK_LIBS)

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
