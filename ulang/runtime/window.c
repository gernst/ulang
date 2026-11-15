// the windows of window.u of the library, of GTK 4, which the runtime opens by dlopen when the first window opens, so
// that neither the build nor a program that opens none needs GTK; included at the end of ulang.c, whose statics it uses.
//
// A window is patched by lines of fields separated by tabs, as gui/protocol.u writes them, and its widgets only queue
// events, as lines of the same kind, which window_next gives, so no code of ulang runs inside a callback of GTK and no
// exception of ulang crosses a frame of it. The widgets are known by the ids of the patches, in a table of malloc, as
// is all that GTK reaches, which the collector does not scan, so no value of ulang is kept there.

#include <dlfcn.h>

typedef int gboolean;
typedef void *gpointer;
typedef void GtkWidget;
typedef void cairo_t;
typedef void (*GCallback)(void);

typedef struct {
  float x, y, width, height;
} graphene_rect_t;

// the functions of GTK, GLib, and cairo that the windows call, each a pointer that gtk_load sets
#define GTK_FUNCTIONS(X)                                                                                               \
  X(gboolean, gtk_init_check, (void))                                                                                  \
  X(GtkWidget *, gtk_window_new, (void))                                                                               \
  X(void, gtk_window_set_title, (GtkWidget *, const char *))                                                           \
  X(void, gtk_window_set_default_size, (GtkWidget *, int, int))                                                        \
  X(void, gtk_window_set_child, (GtkWidget *, GtkWidget *))                                                            \
  X(void, gtk_window_present, (GtkWidget *))                                                                           \
  X(void, gtk_window_destroy, (GtkWidget *))                                                                           \
  X(GtkWidget *, gtk_label_new, (const char *))                                                                        \
  X(void, gtk_label_set_text, (GtkWidget *, const char *))                                                             \
  X(const char *, gtk_label_get_text, (GtkWidget *))                                                                   \
  X(GtkWidget *, gtk_button_new, (void))                                                                               \
  X(void, gtk_button_set_child, (GtkWidget *, GtkWidget *))                                                            \
  X(GtkWidget *, gtk_check_button_new, (void))                                                                         \
  X(void, gtk_check_button_set_child, (GtkWidget *, GtkWidget *))                                                      \
  X(void, gtk_check_button_set_active, (GtkWidget *, gboolean))                                                        \
  X(gboolean, gtk_check_button_get_active, (GtkWidget *))                                                              \
  X(GtkWidget *, gtk_entry_new, (void))                                                                                \
  X(const char *, gtk_editable_get_text, (GtkWidget *))                                                                \
  X(void, gtk_editable_set_text, (GtkWidget *, const char *))                                                          \
  X(GtkWidget *, gtk_box_new, (int, int))                                                                              \
  X(void, gtk_box_set_spacing, (GtkWidget *, int))                                                                     \
  X(void, gtk_box_insert_child_after, (GtkWidget *, GtkWidget *, GtkWidget *))                                         \
  X(void, gtk_box_remove, (GtkWidget *, GtkWidget *))                                                                  \
  X(GtkWidget *, gtk_drawing_area_new, (void))                                                                         \
  X(void, gtk_drawing_area_set_draw_func,                                                                              \
    (GtkWidget *, void (*)(GtkWidget *, cairo_t *, int, int, gpointer), gpointer, void (*)(gpointer)))                 \
  X(void, gtk_drawing_area_set_content_width, (GtkWidget *, int))                                                      \
  X(void, gtk_drawing_area_set_content_height, (GtkWidget *, int))                                                     \
  X(void, gtk_widget_set_size_request, (GtkWidget *, int, int))                                                        \
  X(void, gtk_widget_set_hexpand, (GtkWidget *, gboolean))                                                             \
  X(void, gtk_widget_set_vexpand, (GtkWidget *, gboolean))                                                             \
  X(void, gtk_widget_set_sensitive, (GtkWidget *, gboolean))                                                           \
  X(void, gtk_widget_set_margin_top, (GtkWidget *, int))                                                               \
  X(void, gtk_widget_set_margin_bottom, (GtkWidget *, int))                                                            \
  X(void, gtk_widget_set_margin_start, (GtkWidget *, int))                                                             \
  X(void, gtk_widget_set_margin_end, (GtkWidget *, int))                                                               \
  X(void, gtk_widget_queue_draw, (GtkWidget *))                                                                        \
  X(int, gtk_widget_get_width, (GtkWidget *))                                                                          \
  X(int, gtk_widget_get_height, (GtkWidget *))                                                                         \
  X(void, g_signal_emit_by_name, (gpointer, const char *, ...))                                                       \
  X(void *, gtk_widget_get_native, (GtkWidget *))                                                                      \
  X(void *, gtk_native_get_renderer, (void *))                                                                         \
  X(void *, gtk_gesture_click_new, (void))                                                                             \
  X(void, gtk_widget_add_controller, (GtkWidget *, void *))                                                            \
  X(void *, gtk_widget_paintable_new, (GtkWidget *))                                                                   \
  X(void *, gtk_snapshot_new, (void))                                                                                  \
  X(void, gdk_paintable_snapshot, (void *, void *, double, double))                                                    \
  X(void *, gtk_snapshot_free_to_node, (void *))                                                                       \
  X(void *, gsk_renderer_render_texture, (void *, void *, const graphene_rect_t *))                                    \
  X(gboolean, gdk_texture_save_to_png, (void *, const char *))                                                         \
  X(void, gsk_render_node_unref, (void *))                                                                             \
  X(void, g_object_unref, (gpointer))                                                                                  \
  X(unsigned long, g_signal_connect_data, (gpointer, const char *, GCallback, gpointer, void (*)(gpointer), int))      \
  X(gboolean, g_main_context_iteration, (void *, gboolean))                                                            \
  X(gboolean, g_main_context_pending, (void *))                                                                        \
  X(void, cairo_set_source_rgb, (cairo_t *, double, double, double))                                                   \
  X(void, cairo_rectangle, (cairo_t *, double, double, double, double))                                                \
  X(void, cairo_arc, (cairo_t *, double, double, double, double, double))                                              \
  X(void, cairo_move_to, (cairo_t *, double, double))                                                                  \
  X(void, cairo_line_to, (cairo_t *, double, double))                                                                  \
  X(void, cairo_set_line_width, (cairo_t *, double))                                                                   \
  X(void, cairo_set_font_size, (cairo_t *, double))                                                                    \
  X(void, cairo_show_text, (cairo_t *, const char *))                                                                  \
  X(void, cairo_fill, (cairo_t *))                                                                                     \
  X(void, cairo_stroke, (cairo_t *))

#define GTK_POINTER(result, name, params) static result(*name##_)params;
GTK_FUNCTIONS(GTK_POINTER)

static int gtk_state = 0;  // 0 not loaded, 1 loaded, -1 failed

static void gtk_load(void) {
  if (gtk_state == 0) {
    void *gtk = dlopen("libgtk-4.so.1", RTLD_NOW | RTLD_GLOBAL);
    gtk_state = gtk ? 1 : -1;
#define GTK_SYMBOL(result, name, params)                                                                               \
  if (gtk_state == 1 && !(*(void **)&name##_ = dlsym(gtk, #name))) gtk_state = -1;
    GTK_FUNCTIONS(GTK_SYMBOL)
    if (gtk_state == 1 && !gtk_init_check_()) gtk_state = -1;
  }
  if (gtk_state < 0) os_error("cannot open a window: GTK 4 or a display is missing", u_mk_lit(""));
}

#define G_CONNECT(widget, signal, handler, data) g_signal_connect_data_(widget, signal, (GCallback)(handler), data, NULL, 0)

enum { K_TEXT, K_BUTTON, K_ENTRY, K_CHECK, K_COLUMN, K_ROW, K_CANVAS };

static const char *kind_names[] = {"text", "button", "entry", "check", "column", "row", "canvas", NULL};

struct shape {
  int kind;  // 0 rect, 1 circle, 2 line, 3 label
  double args[4];
  double rgb[3];
  char *text;
};

struct widget {
  int kind;
  long id, parent;
  GtkWidget *widget, *content;
  long *children;
  int count, room;
  struct shape *shapes;
  int shape_count;
  struct window *window;
};

struct window {
  GtkWidget *gtk;
  struct widget **widgets;  // by id
  long room;
  char **events;  // a queue of lines
  int first, last, events_room;
  int quiet, closed;
  FILE *script;
};

static char *copy_text(const char *s) {
  size_t n = strlen(s) + 1;
  char *c = malloc(n);
  memcpy(c, s, n);
  return c;
}

// a field without tabs, newlines, and backslashes but as escapes, as escaped of gui/protocol.u
static char *escaped(const char *s) {
  char *out = malloc(2 * strlen(s) + 1), *o = out;
  for (; *s; s++) {
    if (*s == '\\' || *s == '\t' || *s == '\n') {
      *o++ = '\\';
      *o++ = *s == '\t' ? 't' : *s == '\n' ? 'n' : '\\';
    } else
      *o++ = *s;
  }
  *o = 0;
  return out;
}

// unescapes a field in place
static char *unescape(char *s) {
  char *o = s;
  for (char *p = s; *p; p++) {
    if (*p == '\\' && p[1]) {
      p++;
      *o++ = *p == 't' ? '\t' : *p == 'n' ? '\n' : *p;
    } else
      *o++ = *p;
  }
  *o = 0;
  return s;
}

static void queue_event(struct window *w, const char *kind, long id, const char *rest) {
  if (w->quiet || w->closed) return;
  char line[64];
  snprintf(line, sizeof line, id ? "%s\t%ld" : "%s", kind, id);
  char *text = malloc(strlen(line) + (rest ? strlen(rest) + 1 : 0) + 1);
  strcpy(text, line);
  if (rest) {
    strcat(text, "\t");
    strcat(text, rest);
  }
  if (w->last == w->events_room) {
    if (w->first > 0) {
      memmove(w->events, w->events + w->first, (w->last - w->first) * sizeof(char *));
      w->last -= w->first;
      w->first = 0;
    } else {
      w->events_room = w->events_room ? 2 * w->events_room : 16;
      w->events = realloc(w->events, w->events_room * sizeof(char *));
    }
  }
  w->events[w->last++] = text;
}

static void on_clicked(GtkWidget *button, struct widget *x) {
  queue_event(x->window, "click", x->id, NULL);
}

static void on_changed(GtkWidget *entry, struct widget *x) {
  char *text = escaped(gtk_editable_get_text_(entry));
  queue_event(x->window, "input", x->id, text);
  free(text);
}

static void on_toggled(GtkWidget *check, struct widget *x) {
  queue_event(x->window, "toggle", x->id, gtk_check_button_get_active_(check) ? "1" : "0");
}

static void on_pressed(void *gesture, int presses, double px, double py, struct widget *x) {
  char at[64];
  snprintf(at, sizeof at, "%d\t%d", (int)px, (int)py);
  queue_event(x->window, "pointer", x->id, at);
}

static gboolean on_close(GtkWidget *gtk, struct window *w) {
  queue_event(w, "close", 0, NULL);
  return 1;
}

static void on_draw(GtkWidget *area, cairo_t *cr, int width, int height, gpointer data) {
  struct widget *x = data;
  for (int i = 0; i < x->shape_count; i++) {
    struct shape *s = &x->shapes[i];
    double *a = s->args;
    cairo_set_source_rgb_(cr, s->rgb[0] / 255, s->rgb[1] / 255, s->rgb[2] / 255);
    if (s->kind == 0) {
      cairo_rectangle_(cr, a[0], a[1], a[2], a[3]);
      cairo_fill_(cr);
    } else if (s->kind == 1) {
      cairo_arc_(cr, a[0], a[1], a[2], 0, 6.283185307179586);
      cairo_fill_(cr);
    } else if (s->kind == 2) {
      cairo_set_line_width_(cr, 2);
      cairo_move_to_(cr, a[0], a[1]);
      cairo_line_to_(cr, a[2], a[3]);
      cairo_stroke_(cr);
    } else {
      cairo_set_font_size_(cr, 14);
      cairo_move_to_(cr, a[0], a[1]);
      cairo_show_text_(cr, s->text);
    }
  }
}

static void window_fail(const char *what, const char *detail) {
  os_error(what, u_mk_str(detail));
}

static struct widget *widget_of(struct window *w, const char *field) {
  long id = atol(field);
  if (id <= 0 || id >= w->room || !w->widgets[id]) window_fail("window: no widget ", field);
  return w->widgets[id];
}

static void forget(struct window *w, struct widget *x) {
  for (int i = 0; i < x->count; i++) forget(w, w->widgets[x->children[i]]);
  for (int i = 0; i < x->shape_count; i++) free(x->shapes[i].text);
  free(x->shapes);
  free(x->children);
  w->widgets[x->id] = NULL;
  free(x);
}

static void create(struct window *w, long id, long parent, int index, const char *kind_name) {
  int kind = 0;
  while (kind_names[kind] && strcmp(kind_names[kind], kind_name)) kind++;
  if (!kind_names[kind]) window_fail("window: no kind ", kind_name);
  struct widget *x = calloc(1, sizeof *x);
  x->kind = kind, x->id = id, x->parent = parent, x->window = w;
  switch (kind) {
    case K_TEXT:
      x->widget = gtk_label_new_("");
      break;
    case K_BUTTON:
      x->widget = gtk_button_new_();
      x->content = gtk_box_new_(0, 0);
      gtk_button_set_child_(x->widget, x->content);
      G_CONNECT(x->widget, "clicked", on_clicked, x);
      break;
    case K_ENTRY:
      x->widget = gtk_entry_new_();
      G_CONNECT(x->widget, "changed", on_changed, x);
      break;
    case K_CHECK:
      x->widget = gtk_check_button_new_();
      x->content = gtk_box_new_(0, 0);
      gtk_check_button_set_child_(x->widget, x->content);
      G_CONNECT(x->widget, "toggled", on_toggled, x);
      break;
    case K_COLUMN:
    case K_ROW:
      x->widget = x->content = gtk_box_new_(kind == K_COLUMN, 0);
      break;
    case K_CANVAS: {
      x->widget = gtk_drawing_area_new_();
      gtk_drawing_area_set_draw_func_(x->widget, on_draw, x, NULL);
      void *gesture = gtk_gesture_click_new_();
      G_CONNECT(gesture, "pressed", on_pressed, x);
      gtk_widget_add_controller_(x->widget, gesture);
      break;
    }
  }
  if (id >= w->room) {
    long room = w->room;
    while (id >= w->room) w->room = w->room ? 2 * w->room : 64;
    w->widgets = realloc(w->widgets, w->room * sizeof *w->widgets);
    memset(w->widgets + room, 0, (w->room - room) * sizeof *w->widgets);
  }
  if (w->widgets[id]) window_fail("window: the widget exists already ", kind_name);
  w->widgets[id] = x;
  if (parent == 0) {
    gtk_widget_set_margin_top_(x->widget, 10);
    gtk_widget_set_margin_bottom_(x->widget, 10);
    gtk_widget_set_margin_start_(x->widget, 10);
    gtk_widget_set_margin_end_(x->widget, 10);
    gtk_window_set_child_(w->gtk, x->widget);
    return;
  }
  char digits[24];
  snprintf(digits, sizeof digits, "%ld", parent);
  struct widget *owner = widget_of(w, digits);
  if (!owner->content || index < 0 || index > owner->count) window_fail("window: no place in ", digits);
  if (owner->count == owner->room) {
    owner->room = owner->room ? 2 * owner->room : 4;
    owner->children = realloc(owner->children, owner->room * sizeof(long));
  }
  memmove(owner->children + index + 1, owner->children + index, (owner->count - index) * sizeof(long));
  owner->children[index] = id;
  owner->count++;
  GtkWidget *sibling = index > 0 ? w->widgets[owner->children[index - 1]]->widget : NULL;
  gtk_box_insert_child_after_(owner->content, x->widget, sibling);
}

static void remove_widget(struct window *w, struct widget *x) {
  if (x->parent == 0)
    gtk_window_set_child_(w->gtk, NULL);
  else {
    struct widget *owner = w->widgets[x->parent];
    int i = 0;
    while (owner->children[i] != x->id) i++;
    memmove(owner->children + i, owner->children + i + 1, (owner->count - i - 1) * sizeof(long));
    owner->count--;
    gtk_box_remove_(owner->content, x->widget);
  }
  forget(w, x);
}

static void set_shapes(struct widget *x, char **fields, int n) {
  for (int i = 0; i < x->shape_count; i++) free(x->shapes[i].text);
  free(x->shapes);
  int count = n > 0 ? atoi(fields[0]) : 0, at = 1;
  x->shapes = calloc(count ? count : 1, sizeof(struct shape));
  x->shape_count = 0;
  for (int i = 0; i < count && at < n; i++) {
    struct shape *s = &x->shapes[x->shape_count++];
    const char *kind = fields[at++];
    int arity = !strcmp(kind, "circle") ? 3 : !strcmp(kind, "label") ? 2 : 4;
    s->kind = !strcmp(kind, "rect") ? 0 : !strcmp(kind, "circle") ? 1 : !strcmp(kind, "line") ? 2 : 3;
    for (int k = 0; k < arity && at < n; k++) s->args[k] = atof(fields[at++]);
    if (s->kind == 3 && at < n) s->text = copy_text(unescape(fields[at++]));
    for (int k = 0; k < 3 && at < n; k++) s->rgb[k] = atof(fields[at++]);
  }
  gtk_widget_queue_draw_(x->widget);
}

static void set_prop(struct window *w, struct widget *x, const char *prop, char **fields, int n) {
  GtkWidget *g = x->widget;
  int on = n > 0 && !strcmp(fields[0], "1");
  if (!strcmp(prop, "text") && x->kind == K_TEXT && n > 0)
    gtk_label_set_text_(g, unescape(fields[0]));
  else if (!strcmp(prop, "value") && x->kind == K_ENTRY && n > 0) {
    unescape(fields[0]);
    if (strcmp(gtk_editable_get_text_(g), fields[0])) gtk_editable_set_text_(g, fields[0]);
  } else if (!strcmp(prop, "checked") && x->kind == K_CHECK)
    gtk_check_button_set_active_(g, on);
  else if (!strcmp(prop, "disabled"))
    gtk_widget_set_sensitive_(g, !on);
  else if (!strcmp(prop, "spacing") && x->content == g && n > 0)
    gtk_box_set_spacing_(g, atoi(fields[0]));
  else if (!strcmp(prop, "size") && n > 1) {
    if (x->kind == K_CANVAS) {
      gtk_drawing_area_set_content_width_(g, atoi(fields[0]) < 0 ? 0 : atoi(fields[0]));
      gtk_drawing_area_set_content_height_(g, atoi(fields[1]) < 0 ? 0 : atoi(fields[1]));
    } else
      gtk_widget_set_size_request_(g, atoi(fields[0]), atoi(fields[1]));
  } else if (!strcmp(prop, "expand")) {
    // along the axis of the box around it, both ways in the window
    struct widget *owner = x->parent ? w->widgets[x->parent] : NULL;
    int vertical = owner && owner->kind == K_COLUMN;
    gtk_widget_set_hexpand_(g, on && !vertical);
    gtk_widget_set_vexpand_(g, on && (vertical || !owner));
  } else if (!strcmp(prop, "shapes") && x->kind == K_CANVAS)
    set_shapes(x, fields, n);
  else
    window_fail("window: a property that the widget lacks, ", prop);
}

static void apply_line(struct window *w, char *line) {
  char *fields[4096];
  int n = 0;
  for (char *p = line; p && n < 4096;) {
    fields[n++] = p;
    p = strchr(p, '\t');
    if (p) *p++ = 0;
  }
  const char *op = fields[0];
  if (!strcmp(op, "title") && n == 2)
    gtk_window_set_title_(w->gtk, unescape(fields[1]));
  else if (!strcmp(op, "create") && n == 5)
    create(w, atol(fields[1]), atol(fields[2]), atoi(fields[3]), fields[4]);
  else if (!strcmp(op, "set") && n >= 3)
    set_prop(w, widget_of(w, fields[1]), fields[2], fields + 3, n - 3);
  else if (!strcmp(op, "remove") && n == 2)
    remove_widget(w, widget_of(w, fields[1]));
  else if (strcmp(op, "commit") && strcmp(op, ""))
    window_fail("window: no patch ", op);
}

static struct window *window_state(u_value window) {
  struct window *w = (struct window *)(void *)window->file;
  if (!w || w->closed) os_error("the window is closed: ", window->path);
  return w;
}

void p_window_open(u_value title, u_value width, u_value height, u_value *window) {
  gtk_load();
  struct window *w = calloc(1, sizeof *w);
  w->gtk = gtk_window_new_();
  gtk_window_set_title_(w->gtk, u_utf8(title, NULL));
  gtk_window_set_default_size_(w->gtk, (int)index_of(width), (int)index_of(height));
  G_CONNECT(w->gtk, "close-request", on_close, w);
  gtk_window_present_(w->gtk);
  const char *script = getenv("ULANG_WINDOW_SCRIPT");
  if (script && !(w->script = fopen(script, "r"))) os_error("cannot read ", u_mk_str(script));
  *window = stream_of(&u_window_type, (FILE *)(void *)w, title);
}

void p_window_apply(u_value window, u_value patches) {
  struct window *w = window_state(window);
  char *text = copy_text(u_utf8(patches, NULL));
  w->quiet = 1;
  for (char *line = text; line;) {
    char *next = strchr(line, '\n');
    if (next) *next++ = 0;
    apply_line(w, line);
    line = next;
  }
  w->quiet = 0;
  free(text);
}

// runs the main loop of GLib for the milliseconds, so that patches are laid out and drawn
static void settle(int milliseconds) {
  struct timespec start, now;
  clock_gettime(CLOCK_MONOTONIC, &start);
  do {
    while (g_main_context_pending_(NULL)) g_main_context_iteration_(NULL, 0);
    struct timespec pause = {0, 5000000};
    nanosleep(&pause, NULL);
    clock_gettime(CLOCK_MONOTONIC, &now);
  } while ((now.tv_sec - start.tv_sec) * 1000 + (now.tv_nsec - start.tv_nsec) / 1000000 < milliseconds);
}

static void shot(struct window *w, const char *path) {
  settle(150);
  GtkWidget *child = NULL;
  for (long id = 1; id < w->room && !child; id++)
    if (w->widgets[id] && w->widgets[id]->parent == 0) child = w->widgets[id]->widget;
  if (!child) window_fail("window: nothing to shoot for ", path);
  int width = gtk_widget_get_width_(child), height = gtk_widget_get_height_(child);
  void *snapshot = gtk_snapshot_new_();
  void *paintable = gtk_widget_paintable_new_(child);
  gdk_paintable_snapshot_(paintable, snapshot, width, height);
  void *node = gtk_snapshot_free_to_node_(snapshot);
  graphene_rect_t area = {0, 0, width, height};
  void *texture = gsk_renderer_render_texture_(gtk_native_get_renderer_(gtk_widget_get_native_(w->gtk)), node, &area);
  int saved = gdk_texture_save_to_png_(texture, path);
  g_object_unref_(texture);
  gsk_render_node_unref_(node);
  g_object_unref_(paintable);
  if (!saved) window_fail("cannot write ", path);
}

static struct widget *nth(struct window *w, int kind, int index) {
  for (long id = 1; id < w->room; id++)
    if (w->widgets[id] && w->widgets[id]->kind == kind && index-- == 0) return w->widgets[id];
  window_fail("window: the script names no such widget of the kind ", kind_names[kind]);
  return NULL;
}

static int has_text(struct window *w, struct widget *x, const char *caption) {
  if (x->kind == K_TEXT && !strcmp(gtk_label_get_text_(x->widget), caption)) return 1;
  for (int i = 0; i < x->count; i++)
    if (has_text(w, w->widgets[x->children[i]], caption)) return 1;
  return 0;
}

// the next command of the script, as the commands of gui/host/host.py, which queues events or writes shots
static void script_step(struct window *w) {
  char line[4096];
  while (w->first == w->last) {
    settle(150);
    if (!fgets(line, sizeof line, w->script)) {
      queue_event(w, "close", 0, NULL);
      return;
    }
    line[strcspn(line, "\n")] = 0;
    char *rest = strchr(line, ' ');
    if (rest) *rest++ = 0;
    else rest = "";
    if (!strcmp(line, "shot"))
      shot(w, rest);
    else if (!strcmp(line, "click")) {
      struct widget *found = NULL;
      for (long id = 1; id < w->room && !found; id++)
        if (w->widgets[id] && w->widgets[id]->kind == K_BUTTON && has_text(w, w->widgets[id], rest)) found = w->widgets[id];
      if (!found) window_fail("window: no button ", rest);
      g_signal_emit_by_name_(found->widget, "clicked");
    } else if (!strcmp(line, "type")) {
      char *text = strchr(rest, ' ');
      if (!text) window_fail("window: type needs an index and a text, not ", rest);
      *text++ = 0;
      gtk_editable_set_text_(nth(w, K_ENTRY, atoi(rest))->widget, text);
    } else if (!strcmp(line, "toggle")) {
      GtkWidget *check = nth(w, K_CHECK, atoi(rest))->widget;
      gtk_check_button_set_active_(check, !gtk_check_button_get_active_(check));
    } else if (!strcmp(line, "point")) {
      int index, x, y;
      if (sscanf(rest, "%d %d %d", &index, &x, &y) != 3) window_fail("window: point needs an index, x, and y, not ", rest);
      char at[64];
      snprintf(at, sizeof at, "%d\t%d", x, y);
      queue_event(w, "pointer", nth(w, K_CANVAS, index)->id, at);
    } else if (!strcmp(line, "close"))
      queue_event(w, "close", 0, NULL);
    else
      window_fail("window: no command ", line);
  }
}

void p_window_next(u_value window, u_value *event) {
  struct window *w = window_state(window);
  while (w->first == w->last) {
    if (w->script)
      script_step(w);
    else
      g_main_context_iteration_(NULL, 1);
  }
  char *line = w->events[w->first++];
  *event = u_from_utf8(line, strlen(line));
  free(line);
}

void p_window_shot(u_value window, u_value path) {
  shot(window_state(window), u_utf8(path, NULL));
}

void p_window_close(u_value window) {
  struct window *w = (struct window *)(void *)window->file;
  if (!w || w->closed) return;
  w->closed = 1;
  gtk_window_destroy_(w->gtk);
  while (g_main_context_pending_(NULL)) g_main_context_iteration_(NULL, 0);
}

static u_value show_window(u_value v) {
  struct u_text t = {NULL, 0, 0, 1};
  append(&t, "<window ");
  append(&t, u_utf8(v->path, NULL));
  append(&t, ">");
  return text_value(&t);
}

const struct u_type u_window_type = {"window.Window", compare_streams, u_equals_by_compare, show_window};
