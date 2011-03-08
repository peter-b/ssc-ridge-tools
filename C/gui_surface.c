#include "config.h"

#include <stdint.h>
#include <math.h>
#include <cairo.h>
#include <gtk/gtk.h>
#include <assert.h>

#include "ridgetool.h"
#include "ridgetool_gui.h"


cairo_surface_t *
surface_to_cairo (Surface *s, float limit)
{
  union crsdata {
    uint32_t v;
    struct {
      uint8_t b;
      uint8_t g;
      uint8_t r;
      uint8_t unused;
    } c;
  };

  cairo_surface_t *crs;
  int i, j, stride;
  float scale;
  uint8_t val8;
  float *srcptr;
  uint8_t *dest;
  uint8_t *destptr;
  union crsdata *components;

  assert (s);

  /* Create Cairo image surface */
  crs = cairo_image_surface_create (CAIRO_FORMAT_RGB24,
                                    s->cols,
                                    s->rows);
  stride = cairo_image_surface_get_stride (crs);
  dest = cairo_image_surface_get_data (crs);

  /* Calculate scale factor */
  scale = 255/limit;

  for (i = 0; i < s->rows; i++) {
    for (j = 0; j < s->cols; j++) {
      int v;
      /* Calculate source and destination pointers */
      srcptr = s->data + i * s->cols + j;
      destptr = dest + i * stride + j * sizeof(union crsdata);
      components = (union crsdata *) destptr;

      /* Calculate 8-bit brightness value */
      v = lrintf (*srcptr * scale);
      v = (v >= 0) ? v : 0;
      val8 = (uint8_t) v;

      /* Assign to RGB components */
      components->c.r = components->c.g = components->c.b = val8;
    }
  }

  return crs;
}

static gboolean
image_widget_expose_event (GtkWidget *widget, GdkEventExpose *event,
                           gpointer user_data) {
  cairo_surface_t *img = (cairo_surface_t *) user_data;
  cairo_t *cr = gdk_cairo_create (event->window);
  GtkAllocation allocation;
  double w, h, s;

  /* Calculate scale factor */
  allocation = widget->allocation;
  w = ((double) allocation.width) / cairo_image_surface_get_width (img);
  h = ((double) allocation.height) / cairo_image_surface_get_height (img);
  s = (h < w) ? h : w;

  cairo_scale (cr, s, s);
  cairo_set_source_surface (cr, img, 0, 0);
  cairo_paint (cr);
  cairo_destroy (cr);
  return TRUE;
}

void
show_surface_dialog (Surface *s, float limit) {
  GtkWidget *dialog, *content_area, *drawable;
  cairo_surface_t *img_surface;

  /* Create & populate dialog */
  dialog = gtk_dialog_new ();
  content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  drawable = gtk_drawing_area_new ();
  gtk_widget_set_size_request (GTK_WIDGET (drawable), s->cols, s->rows);
  gtk_container_add (GTK_CONTAINER (content_area), drawable);
  gtk_widget_show (drawable);

  /* Create Cairo surface and hook up events */
  img_surface = surface_to_cairo (s, limit);
  gtk_widget_set_events (drawable, GDK_EXPOSURE_MASK);
  g_signal_connect (G_OBJECT (drawable), "expose-event",
                    G_CALLBACK (image_widget_expose_event),
                    img_surface);

  /* Show dialog */
  gtk_dialog_run (GTK_DIALOG (dialog));

  /* Clean up */
  gtk_widget_destroy (dialog);
  cairo_surface_destroy (img_surface);
}
