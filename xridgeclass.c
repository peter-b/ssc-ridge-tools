/*
 * Surrey Space Centre ridge tools for SAR data processing
 * Copyright (C) 2011-2013  Peter Brett <p.brett@surrey.ac.uk>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "config.h"

#include <gtk/gtk.h>

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <ridgeio.h>

#define GETOPT_OPTIONS "h"

static void
usage (char *name, int status)
{
  printf (
"Usage: %s [FILE]\n"
"\n"
"Edit ridge data file class labels.\n"
"\n"
"  -h              Display this message and exit\n"
"\n"
"Displays a graphical interface for editing classification metadata\n"
"attached to ridge data files.  If a FILE is specified, loads and\n"
"displays it for editing at startup.\n"
"\n"
"Please report bugs to %s.\n",
name, PACKAGE_BUGREPORT);

  exit (status);
}

/* ------------------------------------------------------ */

/* Domain for errors */
#define XRC_ERROR xrc_error_quark()

GQuark
xrc_error_quark ()
{
  return g_quark_from_static_string ("xrc-error-quark");
}

/* Error numbers */
typedef enum {
  XRC_ERROR_DATA_TYPE,
  XRC_ERROR_DATA_METADATA,
} XrcError;

/* ------------------------------------------------------ */

RioData *active_data = NULL;
gchar *active_filename = NULL;
guint32 active_rows = 300;
guint32 active_cols = 400;
guint8 *active_class = NULL;
gboolean active_changed = FALSE;
int selected_index = -1;
int *active_map = NULL;

GtkWidget *view = NULL;

/* ------------------------------------------------------ */

void
close_data ()
{
  rio_data_destroy (active_data);
  active_data = NULL;

  g_free (active_filename);
  active_filename = NULL;

  g_free (active_map);
  active_map = NULL;

  active_class = NULL;
  active_changed = FALSE;
}

gboolean
load_data (const gchar *filename, GError **err)
{
  g_return_val_if_fail (active_data == NULL, FALSE);
  g_return_val_if_fail (filename != NULL, FALSE);

  RioData *d = rio_data_from_file (filename);
  if (d == NULL) {
    return FALSE;
  }

  /* Check that the file contains line data */
  if (rio_data_get_type (d) != RIO_DATA_LINES) {
    rio_data_destroy (d);
    g_set_error (err, XRC_ERROR, XRC_ERROR_DATA_TYPE,
                 "%s does not contain ridge line data",
                 filename);
    return FALSE;
  }

  /* Get number of rows and columns */
  if (!(rio_data_get_metadata_uint32 (d, RIO_KEY_IMAGE_ROWS,
                                      &active_rows)
        && rio_data_get_metadata_uint32 (d, RIO_KEY_IMAGE_COLS,
                                         &active_cols))) {
    rio_data_destroy (d);
    g_set_error (err, XRC_ERROR, XRC_ERROR_DATA_METADATA,
                 "%s does not contain image size metadata",
                 filename);
    return FALSE;
  }

  /* Create a classification metadata entry if necessary */
  size_t class_data_size;
  active_class =
    (guint8 *) rio_data_get_metadata (d, RIO_KEY_IMAGE_CLASSIFICATION,
                                      &class_data_size);
  if (active_class == NULL) {
    /* Classification metadata missing, create it */
    class_data_size = rio_data_get_num_entries (d);
    active_class = malloc (class_data_size);
    memset (active_class, 0, class_data_size);
    rio_data_take_metadata (d, RIO_KEY_IMAGE_CLASSIFICATION,
                            (char *) active_class, class_data_size);

  } else if (class_data_size < rio_data_get_num_entries (d)) {
    /* Classification metadata too short, extend it */
    size_t new_class_data_size = rio_data_get_num_entries (d);
    guint8 *new_class = malloc (new_class_data_size);
    memcpy (new_class, active_class, class_data_size);
    memset (new_class + class_data_size, 0,
            new_class_data_size - class_data_size);
    class_data_size = new_class_data_size;
    active_class = new_class;
    rio_data_take_metadata (d, RIO_KEY_IMAGE_CLASSIFICATION,
                            (char *) active_class,
                            class_data_size);
  }

  /* Build a map for reverse line position lookups */
  size_t map_size = sizeof (int) * active_rows * active_cols;
  active_map = g_malloc0 (map_size);
  for (int i = 0; i < rio_data_get_num_entries (d); i++) {
    RioLine *l = rio_data_get_line (d, i);
    for (int j = 0; j + 1 < rio_line_get_length (l); j++) {
      RioPoint *p = rio_line_get_point (l, j);
      RioPoint *q = rio_line_get_point (l, j + 1);
      int r1, c1, r2, c2;
      rio_point_get_position (p, &r1, &c1);
      rio_point_get_position (q, &r2, &c2);
      /* Find the top left coordinates of the pixel containing the jth
       * segment */
      r1 = (r1 + r2) >> 8;
      c1 = (c1 + c2) >> 8;
      active_map[active_cols * r1 + c1] = i;
    }
  }

  active_data = d;
  active_filename = g_strdup (filename);
  active_changed = FALSE;
  return TRUE;
}

gboolean
save_data (const gchar *filename)
{
  if (filename == NULL) {
    filename = active_filename;
  }

  if (!rio_data_to_file (active_data, filename)) return FALSE;
  active_changed = FALSE;

  return TRUE;
}

/* ------------------------------------------------------ */

double
surface_widget_get_scale_factor (GtkWidget *widget)
{
  double w, h, s;
  GtkAllocation allocation = widget->allocation;
  w = ((double) allocation.width) / active_cols;
  h = ((double) allocation.height) / active_rows;
  s = (h < w) ? h : w;
  return s;
}


static gboolean
surface_widget_expose_event (GtkWidget *widget, GdkEventExpose *event,
                             gpointer user_data)
{
  cairo_t *cr;
  double s;

  cr = gdk_cairo_create (event->window);

  /* Background colour */
  cairo_set_source_rgb (cr, 1, 1, 1);
  cairo_paint (cr);

  if (active_data == NULL) {
    return TRUE;
  }

  /* Calculate scale factor */
  s = surface_widget_get_scale_factor (widget);

  cairo_scale (cr, s, s);
  cairo_set_line_width (cr, 0.5);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);

  /* Draw lines */
  for (int j = 0; j < rio_data_get_num_entries (active_data); j++) {
    if (j == selected_index) {
      cairo_set_source_rgb (cr, 0, 1, 0);
    } else if (active_class[j]) {
      cairo_set_source_rgb (cr, 1, 0, 0);
    } else {
      cairo_set_source_rgb (cr, 0, 0, 1);
    }

    RioLine *l = rio_data_get_line (active_data, j);
    RioPoint *prev = NULL, *curr;
    for (int i = 0; i < rio_line_get_length (l); i++) {
      curr = rio_line_get_point (l, i);
      double x, y;
      rio_point_get_subpixel (curr, &y, &x);
      if (prev != NULL) {
        cairo_line_to (cr, x, y);
      } else {
        cairo_move_to (cr, x, y);
      }
      prev = curr;
    }

    if (j == selected_index) {
      cairo_save (cr);
      cairo_set_line_width (cr, 1);
      cairo_set_source_rgb (cr, 0, 1, 0);
      cairo_stroke_preserve (cr);
      cairo_restore (cr);
    }

    if (active_class[j]) {
      cairo_set_source_rgb (cr, 1, 0, 0);
    } else {
      cairo_set_source_rgb (cr, 0, 0, 1);
    }
    cairo_stroke (cr);
  }

  cairo_destroy (cr);
  return FALSE;
}

gboolean
surface_widget_button_press_event (GtkWidget *widget, GdkEventButton *event,
                                   gpointer user_data)
{
  if (active_data == NULL) return TRUE;

  if (event->button == 3 && selected_index >= 0
      && selected_index < rio_data_get_num_entries (active_data)) {
    /* Change class */
    active_class[selected_index] = !active_class[selected_index];
    active_changed = TRUE;

  } else if (event->button == 1) {
    /* Change selection */
    double s = surface_widget_get_scale_factor (widget);
    double x = event->x / s;
    double y = event->y / s;

    if (x < 0 || x >= active_cols || y < 0 || y >= active_rows) {
      selected_index = -1;
    } else {
      selected_index = active_map[lrint (y) * active_cols + lrint (x)];
    }
  }

  gtk_widget_queue_draw (view);

  return TRUE;
}

GtkWidget *
create_surface_widget ()
{
  GtkWidget *drawable;

  drawable = gtk_drawing_area_new ();
  gtk_widget_set_events (drawable, GDK_EXPOSURE_MASK | GDK_BUTTON_PRESS_MASK);
  g_signal_connect (G_OBJECT (drawable), "expose-event",
                    G_CALLBACK (surface_widget_expose_event),
                    NULL);
  g_signal_connect (G_OBJECT (drawable), "button-press-event",
                    G_CALLBACK (surface_widget_button_press_event),
                    NULL);
  gtk_widget_set_size_request (GTK_WIDGET (drawable), active_cols, active_rows);

  return drawable;
}

gboolean
query_discard_changes (GtkWindow *parent)
{
  if (!active_data || !active_changed) return TRUE;

  GtkWidget *dialog =
    gtk_message_dialog_new (parent,
                            GTK_DIALOG_MODAL,
                            GTK_MESSAGE_QUESTION,
                            GTK_BUTTONS_NONE,
                            "You have unsaved changes that will be lost if you continue.");

  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                            "Would you like to discard them?");

  gtk_dialog_add_buttons (GTK_DIALOG (dialog),
                          "Discard changes", GTK_RESPONSE_OK,
                          "Cancel", GTK_RESPONSE_CANCEL,
                          NULL);

  gint response = gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);

  return (response == GTK_RESPONSE_OK) ? TRUE : FALSE;
}

gboolean
event_delete (GtkWindow *window, GdkEvent *event, gpointer user_data)
{
  if (!query_discard_changes (window)) return TRUE;

  close_data ();
  gtk_main_quit ();
  return FALSE;
}

void
event_quit (GtkToolButton *toolbutton, gpointer user_data)
{
  event_delete (GTK_WINDOW (user_data), NULL, NULL);
}

void
event_load (GtkToolButton *toolbutton, gpointer user_data)
{
  GtkWindow *window = GTK_WINDOW (user_data);

  GtkWidget *filechooser =
    gtk_file_chooser_dialog_new ("Open ridge data...", window,
                                 GTK_FILE_CHOOSER_ACTION_OPEN,
                                 GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                 GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT,
                                 NULL);
  if (gtk_dialog_run (GTK_DIALOG (filechooser)) == GTK_RESPONSE_ACCEPT
      && query_discard_changes (window)) {

    close_data ();
    gchar *filename =
      gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (filechooser));
    load_data (filename, NULL); /* FIXME error handling */
    g_free (filename);
  }

  gtk_widget_destroy (filechooser);

  gtk_widget_queue_draw (view);
}

void
event_save (GtkToolButton *toolbutton, gpointer user_data)
{
  if (!active_data) return;
  save_data (NULL); /* FIXME error handling */
}

void
event_save_as (GtkToolButton *toolbutton, gpointer user_data)
{
  GtkWindow *window = GTK_WINDOW (user_data);

  if (!active_data) return;

  GtkWidget *filechooser =
    gtk_file_chooser_dialog_new ("Save ridge data...", window,
                                 GTK_FILE_CHOOSER_ACTION_SAVE,
                                 GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL,
                                 GTK_STOCK_SAVE_AS, GTK_RESPONSE_ACCEPT,
                                 NULL);
  if (gtk_dialog_run (GTK_DIALOG (filechooser)) == GTK_RESPONSE_ACCEPT) {
    gchar *filename =
      gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (filechooser));
    save_data (filename); /* FIXME error handling */
    g_free (active_filename);
    active_filename = filename;
  }

  gtk_widget_destroy (filechooser);
}

void
event_data_changed ()
{
  gtk_widget_set_size_request (view, active_cols, active_rows);
  gtk_widget_queue_draw (view);
}

GtkWidget *
create_window ()
{
  GtkWidget *window, *layout, *toolbar, *label;
  GtkToolItem *toolitem;

  /* Create & populate window */
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_resizable (GTK_WINDOW (window), TRUE);
  g_signal_connect (G_OBJECT (window), "delete-event",
                    G_CALLBACK (event_delete),
                    NULL);

  layout = gtk_vbox_new (FALSE, 2);
  gtk_container_add (GTK_CONTAINER (window), layout);

  toolbar = gtk_toolbar_new ();
  gtk_box_pack_start (GTK_BOX (layout), toolbar, FALSE, TRUE, 0);

  /* Quit button */
  toolitem = gtk_tool_button_new_from_stock (GTK_STOCK_QUIT);
  g_signal_connect (G_OBJECT (toolitem), "clicked",
                    G_CALLBACK (event_quit),
                    window);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), toolitem, -1);

  /* Load button */
  toolitem = gtk_tool_button_new_from_stock (GTK_STOCK_OPEN);
  g_signal_connect (G_OBJECT (toolitem), "clicked",
                    G_CALLBACK (event_load),
                    window);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), toolitem, -1);

  /* Save button */
  toolitem = gtk_tool_button_new_from_stock (GTK_STOCK_SAVE);
  g_signal_connect (G_OBJECT (toolitem), "clicked",
                    G_CALLBACK (event_save),
                    window);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), toolitem, -1);

  /* Save as button */
  toolitem = gtk_tool_button_new_from_stock (GTK_STOCK_SAVE_AS);
  g_signal_connect (G_OBJECT (toolitem), "clicked",
                    G_CALLBACK (event_save_as),
                    window);
  gtk_toolbar_insert (GTK_TOOLBAR (toolbar), toolitem, -1);

  /* View area */
  view = create_surface_widget ();
  gtk_box_pack_start (GTK_BOX (layout), view, TRUE, TRUE, 0);

  /* Explanatory label */
  label = gtk_label_new ("Left click to select. Right click to cycle class.");
  gtk_box_pack_start (GTK_BOX (layout), label, FALSE, TRUE, 0);

  gtk_widget_show_all (layout);

  return window;
}

int
main (int argc, char **argv)
{
  gtk_init (&argc, &argv);
  GtkWidget *window = create_window ();
  int c;

  /* Parse command-line arguments */
  while ((c = getopt (argc, argv, GETOPT_OPTIONS)) != -1) {
    switch (c) {
    case 'h':
      usage (argv[0], 0);
      break;
    case '?':
      if ((optopt != ':') && (strchr (GETOPT_OPTIONS, optopt) != NULL)) {
        fprintf (stderr, "ERROR: -%c option requires an argument.\n\n", optopt);
      } else if (isprint (optopt)) {
        fprintf (stderr, "ERROR: Unknown option -%c.\n\n", optopt);
      } else {
        fprintf (stderr, "ERROR: Unknown option character '\\x%x'.\n\n",
                 optopt);
      }
      usage (argv[0], 1);
    default:
      abort ();
    }
  }

  if (argc - optind > 0) {
    load_data (argv[optind], NULL); /* FIXME error handling */
  }

  gtk_widget_show_all (window);
  gtk_main ();
  return 0;
}
