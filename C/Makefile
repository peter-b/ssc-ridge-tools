CC = gcc

GTK_CFLAGS = $(shell pkg-config --cflags gtk+-2.0)
GTK_LIBS = $(shell pkg-config --libs gtk+-2.0)

CFLAGS = -g -O2 -Wall -std=gnu99 -pedantic $(GTK_CFLAGS)
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
	eigen.c \

show_tiff_SOURCES = \
	multiproc.c \
	show-tiff.c \
	surface.c \
	gui_surface.c

all: ridgetool qr-symmv show-tiff

ridgetool: $(patsubst %.c,%.o,$(ridgetool_SOURCES))

qr-symmv: $(patsubst %.c,%.o,$(qr_symmv_SOURCES))

show-tiff: $(patsubst %.c,%.o,$(show_tiff_SOURCES))

clean:
	-rm ridgetool qr-symmv show-tiff *.o
