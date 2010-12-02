#include <gtk/gtk.h>

#include "ridgetool.h"
#include "ridgetool_gui.h"

static gboolean
expose_event (GtkWidget *widget, GdkEventExpose *event,
              gpointer user_data) {
  cairo_surface_t *img = (cairo_surface_t *) user_data;
  cairo_t *cr = gdk_cairo_create (event->window);
  cairo_set_source_surface (cr, img, 0, 0);
  cairo_paint (cr);
  cairo_destroy (cr);
  return TRUE;
}

void
show_image_dialog (Surface *s, float limit) {
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
                    G_CALLBACK (expose_event), img_surface);

  /* Show dialog */
  gtk_dialog_run (GTK_DIALOG (dialog));

  /* Clean up */
  gtk_widget_destroy (dialog);
  cairo_surface_destroy (img_surface);
}
