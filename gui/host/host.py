#!/usr/bin/env python3
"""The host of the window of a ulang program of gui/: it listens on a port, starts the program with the port as its
argument, creates and patches GTK 4 widgets by the lines that the program sends, applied at each "commit", and sends
the events of the widgets back, one line each, without ever waiting for the program.

    host.py [--script FILE] PROGRAM.u | EXECUTABLE

A .u program runs by out/ulang.sh run of this worktree, anything else is executed with the port as its argument.
--script reads lines of commands, one after each commit of the program, for runs without a person:
    click CAPTION        a click on the first button that has a text of the caption
    type N TEXT          TEXT typed into the N-th entry, from 0
    toggle N             a click on the N-th check
    point N X Y          a click on the N-th canvas at the pixel
    shot FILE            a PNG of the window, then the next command at once
    close                closes the window
"""

import os
import socket
import subprocess
import sys

import gi

gi.require_version("Gtk", "4.0")
gi.require_version("Graphene", "1.0")
from gi.repository import GLib, Graphene, Gtk  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def unescape(field):
    out, i = [], 0
    while i < len(field):
        c = field[i]
        if c == "\\" and i + 1 < len(field):
            n = field[i + 1]
            out.append({"t": "\t", "n": "\n"}.get(n, n))
            i += 2
        else:
            out.append(c)
            i += 1
    return "".join(out)


def escape(text):
    return text.replace("\\", "\\\\").replace("\t", "\\t").replace("\n", "\\n")


class Node:
    def __init__(self, kind, widget, content, parent):
        self.kind, self.widget, self.content, self.parent = kind, widget, content, parent
        self.children = []
        self.shapes = []


class Host:
    def __init__(self, conn, script):
        self.conn = conn
        self.script = script
        self.nodes = {}
        self.pending = b""
        self.batch = []
        self.quiet = False
        self.loop = GLib.MainLoop()
        self.window = Gtk.Window()
        self.window.set_default_size(320, 200)
        self.window.connect("close-request", self.on_close)
        self.window.present()
        conn.setblocking(False)
        GLib.io_add_watch(conn.fileno(), GLib.PRIORITY_DEFAULT, GLib.IO_IN | GLib.IO_HUP, self.on_data)

    # events

    def send(self, *fields):
        if not self.quiet:
            self.conn.sendall(("\t".join(fields) + "\n").encode())

    def on_close(self, _window):
        self.send("close")
        return True

    def on_data(self, _fd, _cond):
        try:
            data = self.conn.recv(65536)
        except BlockingIOError:
            return True
        if not data:
            self.loop.quit()
            return False
        self.pending += data
        while b"\n" in self.pending:
            line, self.pending = self.pending.split(b"\n", 1)
            fields = line.decode().split("\t")
            if fields == ["commit"]:
                for op in self.batch:
                    self.apply(op)
                self.batch = []
                if self.script is not None:
                    GLib.timeout_add(150, self.step)
            else:
                self.batch.append(fields)
        return True

    # patches

    def apply(self, fields):
        op = fields[0]
        if op == "title":
            self.window.set_title(unescape(fields[1]))
        elif op == "create":
            self.create(int(fields[1]), int(fields[2]), int(fields[3]), fields[4])
        elif op == "set":
            self.set(self.nodes[int(fields[1])], fields[2], fields[3:])
        elif op == "remove":
            self.remove(int(fields[1]))
        else:
            raise ValueError("unknown patch " + "\t".join(fields))

    def create(self, id, parent, index, kind):
        content = None
        if kind == "text":
            widget = Gtk.Label()
        elif kind == "button":
            widget, content = Gtk.Button(), Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL)
            widget.set_child(content)
            widget.connect("clicked", lambda _b: self.send("click", str(id)))
        elif kind == "entry":
            widget = Gtk.Entry()
            widget.connect("changed", lambda e: self.send("input", str(id), escape(e.get_text())))
        elif kind == "check":
            widget, content = Gtk.CheckButton(), Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL)
            widget.set_child(content)
            widget.connect("toggled", lambda c: self.send("toggle", str(id), "1" if c.get_active() else "0"))
        elif kind in ("column", "row"):
            orientation = Gtk.Orientation.VERTICAL if kind == "column" else Gtk.Orientation.HORIZONTAL
            widget = Gtk.Box(orientation=orientation)
            content = widget
        elif kind == "canvas":
            widget = Gtk.DrawingArea()
            node = None
            widget.set_draw_func(lambda _a, cr, _w, _h: self.draw(self.nodes.get(id), cr))
            gesture = Gtk.GestureClick()
            gesture.connect("pressed", lambda _g, _n, x, y: self.send("pointer", str(id), str(int(x)), str(int(y))))
            widget.add_controller(gesture)
        else:
            raise ValueError("unknown kind " + kind)
        node = Node(kind, widget, content, parent)
        self.nodes[id] = node
        if parent == 0:
            for side in ("top", "bottom", "start", "end"):
                getattr(widget, "set_margin_" + side)(10)
            self.window.set_child(widget)
        else:
            owner = self.nodes[parent]
            owner.children.insert(index, id)
            sibling = self.nodes[owner.children[index - 1]].widget if index > 0 else None
            owner.content.insert_child_after(widget, sibling)

    def remove(self, id):
        node = self.nodes[id]
        if node.parent == 0:
            self.window.set_child(None)
        else:
            owner = self.nodes[node.parent]
            owner.children.remove(id)
            owner.content.remove(node.widget)
        self.forget(id)

    def forget(self, id):
        for child in self.nodes[id].children:
            self.forget(child)
        del self.nodes[id]

    def set(self, node, prop, values):
        widget = node.widget
        self.quiet = True
        try:
            if prop == "text":
                widget.set_text(unescape(values[0]))
            elif prop == "value":
                if widget.get_text() != unescape(values[0]):
                    widget.set_text(unescape(values[0]))
            elif prop == "checked":
                widget.set_active(values[0] == "1")
            elif prop == "disabled":
                widget.set_sensitive(values[0] != "1")
            elif prop == "spacing":
                widget.set_spacing(int(values[0]))
            elif prop == "size":
                width, height = int(values[0]), int(values[1])
                if node.kind == "canvas":
                    widget.set_content_width(max(width, 0))
                    widget.set_content_height(max(height, 0))
                else:
                    widget.set_size_request(width, height)
            elif prop == "expand":
                # along the axis of the box around it, both ways in the window
                on = values[0] == "1"
                owner = self.nodes.get(node.parent)
                vertical = owner is not None and owner.kind == "column"
                widget.set_hexpand(on and not vertical)
                widget.set_vexpand(on and (vertical or owner is None))
            elif prop == "shapes":
                node.shapes = parse_shapes(values)
                widget.queue_draw()
            else:
                raise ValueError("unknown property " + prop)
        finally:
            self.quiet = False

    def draw(self, node, cr):
        if node is None:
            return
        for shape in node.shapes:
            kind, args, (r, g, b) = shape
            cr.set_source_rgb(r / 255, g / 255, b / 255)
            if kind == "rect":
                cr.rectangle(*args)
                cr.fill()
            elif kind == "circle":
                x, y, radius = args
                cr.arc(x, y, radius, 0, 6.2832)
                cr.fill()
            elif kind == "line":
                x, y, to_x, to_y = args
                cr.set_line_width(2)
                cr.move_to(x, y)
                cr.line_to(to_x, to_y)
                cr.stroke()
            elif kind == "label":
                x, y, text = args
                cr.set_font_size(14)
                cr.move_to(x, y)
                cr.show_text(text)

    # scripts

    def step(self):
        while True:
            line = self.script.readline()
            if not line:
                self.send("close")
                return False
            words = line.rstrip("\n").split(" ", 1)
            command, rest = words[0], (words[1] if len(words) > 1 else "")
            if command == "shot":
                self.shot(rest)
                continue
            if command == "click":
                id = self.find("button", lambda id: self.has_text(id, rest))
                self.nodes[id].widget.emit("clicked")
            elif command == "type":
                index, text = rest.split(" ", 1)
                self.nth("entry", int(index)).widget.set_text(text)
            elif command == "toggle":
                widget = self.nth("check", int(rest)).widget
                widget.set_active(not widget.get_active())
            elif command == "point":
                index, x, y = rest.split(" ")
                id = [i for i, n in sorted(self.nodes.items()) if n.kind == "canvas"][int(index)]
                self.send("pointer", str(id), x, y)
            elif command == "close":
                self.send("close")
            else:
                raise ValueError("unknown command " + command)
            return False

    def has_text(self, id, caption):
        node = self.nodes[id]
        if node.kind == "text" and node.widget.get_text() == caption:
            return True
        return any(self.has_text(child, caption) for child in node.children)

    def find(self, kind, pred):
        for id, node in sorted(self.nodes.items()):
            if node.kind == kind and pred(id):
                return id
        raise ValueError("no %s found" % kind)

    def nth(self, kind, index):
        return [n for _, n in sorted(self.nodes.items()) if n.kind == kind][index]

    def shot(self, path):
        widget = self.window.get_child()
        width, height = widget.get_width(), widget.get_height()
        snapshot = Gtk.Snapshot()
        Gtk.WidgetPaintable.new(widget).snapshot(snapshot, width, height)
        node = snapshot.to_node()
        renderer = self.window.get_native().get_renderer()
        texture = renderer.render_texture(node, Graphene.Rect().init(0, 0, width, height))
        texture.save_to_png(path)


def parse_shapes(values):
    count, rest, shapes = int(values[0]), values[1:], []
    for _ in range(count):
        kind = rest[0]
        if kind == "label":
            args, rest = [int(rest[1]), int(rest[2]), unescape(rest[3])], rest[4:]
        else:
            arity = {"rect": 4, "circle": 3, "line": 4}[kind]
            args, rest = [int(v) for v in rest[1:1 + arity]], rest[1 + arity:]
        color, rest = tuple(int(v) for v in rest[:3]), rest[3:]
        shapes.append((kind, args, color))
    return shapes


def main():
    args = sys.argv[1:]
    script = None
    if len(args) >= 2 and args[0] == "--script":
        script = open(args[1])
        args = args[2:]
    if len(args) != 1:
        print(__doc__, file=sys.stderr)
        return 2
    listener = socket.socket()
    listener.bind(("127.0.0.1", 0))
    listener.listen(1)
    port = str(listener.getsockname()[1])
    program = args[0]
    if program.endswith(".u"):
        command = [os.path.join(ROOT, "out", "ulang.sh"), "run", program, "--", port]
    else:
        command = [program, port]
    child = subprocess.Popen(command)
    listener.settimeout(60)
    conn, _ = listener.accept()
    host = Host(conn, script)
    host.loop.run()
    conn.close()
    return child.wait()


if __name__ == "__main__":
    sys.exit(main())
