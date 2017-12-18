# -*- coding: utf-8 -*-

# drawille is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# drawille is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with drawille. If not, see < http://www.gnu.org/licenses/ >.
#
# (C) 2014- by Adam Tauber, <asciimoo@gmail.com>

import math
import os
from sys import version_info
from collections import defaultdict
from time import sleep
import curses

IS_PY3 = version_info[0] == 3

if IS_PY3:
    unichr = chr

"""

http://www.alanwood.net/unicode/braille_patterns.html

dots:
   ,___,
   |1 4|
   |2 5|
   |3 6|
   |7 8|
   `````
"""

pixel_map = ((0x01, 0x08),
             (0x02, 0x10),
             (0x04, 0x20),
             (0x40, 0x80))

# braille unicode characters starts at 0x2800
braille_char_offset = 0x2800


# http://stackoverflow.com/questions/566746/how-to-get-console-window-width-in-python
def getTerminalSize():
    """Returns terminal width, height
    """
    env = os.environ

    def ioctl_GWINSZ(fd):
        try:
            import fcntl, termios, struct
            cr = struct.unpack('hh', fcntl.ioctl(fd, termios.TIOCGWINSZ, '1234'))
        except:
            return
        return cr

    cr = ioctl_GWINSZ(0) or ioctl_GWINSZ(1) or ioctl_GWINSZ(2)

    if not cr:
        try:
            fd = os.open(os.ctermid(), os.O_RDONLY)
            cr = ioctl_GWINSZ(fd)
            os.close(fd)
        except:
            pass

    if not cr:
        cr = (env.get('LINES', 25), env.get('COLUMNS', 80))

    return int(cr[1]), int(cr[0])


def normalize(coord):
    coord_type = type(coord)

    if coord_type == int:
        return coord
    elif coord_type == float:
        return int(round(coord))
    else:
        raise TypeError("Unsupported coordinate type <{0}>".format(type(coord)))


def intdefaultdict():
    return defaultdict(int)


def get_pos(x, y):
    """Convert x, y to cols, rows"""
    return normalize(x) // 2, normalize(y) // 4


class Canvas(object):
    """This class implements the pixel surface."""

    def __init__(self, line_ending=os.linesep):
        super(Canvas, self).__init__()
        self.clear()
        self.line_ending = line_ending


    def clear(self):
        """Remove all pixels from the :class:`Canvas` object."""
        self.chars = defaultdict(intdefaultdict)


    def set(self, x, y):
        """Set a pixel of the :class:`Canvas` object.

        :param x: x coordinate of the pixel
        :param y: y coordinate of the pixel
        """
        x = normalize(x)
        y = normalize(y)
        col, row = get_pos(x, y)

        if type(self.chars[row][col]) != int:
            return

        self.chars[row][col] |= pixel_map[y % 4][x % 2]


    def unset(self, x, y):
        """Unset a pixel of the :class:`Canvas` object.

        :param x: x coordinate of the pixel
        :param y: y coordinate of the pixel
        """
        x = normalize(x)
        y = normalize(y)
        col, row = get_pos(x, y)

        if type(self.chars[row][col]) == int:
            self.chars[row][col] &= ~pixel_map[y % 4][x % 2]

        if type(self.chars[row][col]) != int or self.chars[row][col] == 0:
            del(self.chars[row][col])

        if not self.chars.get(row):
            del(self.chars[row])


    def toggle(self, x, y):
        """Toggle a pixel of the :class:`Canvas` object.

        :param x: x coordinate of the pixel
        :param y: y coordinate of the pixel
        """
        x = normalize(x)
        y = normalize(y)
        col, row = get_pos(x, y)

        if type(self.chars[row][col]) != int or self.chars[row][col] & pixel_map[y % 4][x % 2]:
            self.unset(x, y)
        else:
            self.set(x, y)


    def set_text(self, x, y, text):
        """Set text to the given coords.

        :param x: x coordinate of the text start position
        :param y: y coordinate of the text start position
        """
        col, row = get_pos(x, y)

        for i,c in enumerate(text):
            self.chars[row][col+i] = c


    def get(self, x, y):
        """Get the state of a pixel. Returns bool.

        :param x: x coordinate of the pixel
        :param y: y coordinate of the pixel
        """
        x = normalize(x)
        y = normalize(y)
        dot_index = pixel_map[y % 4][x % 2]
        col, row = get_pos(x, y)
        char = self.chars.get(row, {}).get(col)

        if not char:
            return False

        if type(char) != int:
            return True

        return bool(char & dot_index)


    def rows(self, min_x=None, min_y=None, max_x=None, max_y=None):
        """Returns a list of the current :class:`Canvas` object lines.

        :param min_x: (optional) minimum x coordinate of the canvas
        :param min_y: (optional) minimum y coordinate of the canvas
        :param max_x: (optional) maximum x coordinate of the canvas
        :param max_y: (optional) maximum y coordinate of the canvas
        """

        if not self.chars.keys():
            return []

        minrow = min_y // 4 if min_y != None else min(self.chars.keys())
        maxrow = (max_y - 1) // 4 if max_y != None else max(self.chars.keys())
        mincol = min_x // 2 if min_x != None else min(min(x.keys()) for x in self.chars.values())
        maxcol = (max_x - 1) // 2 if max_x != None else max(max(x.keys()) for x in self.chars.values())
        ret = []

        for rownum in range(minrow, maxrow+1):
            if not rownum in self.chars:
                ret.append('')
                continue

            maxcol = (max_x - 1) // 2 if max_x != None else max(self.chars[rownum].keys())
            row = []

            for x in  range(mincol, maxcol+1):
                char = self.chars[rownum].get(x)

                if not char:
                    row.append(' ')
                elif type(char) != int:
                    row.append(char)
                else:
                    row.append(unichr(braille_char_offset+char))

            ret.append(''.join(row))

        return ret


    def frame(self, min_x=None, min_y=None, max_x=None, max_y=None):
        """String representation of the current :class:`Canvas` object pixels.

        :param min_x: (optional) minimum x coordinate of the canvas
        :param min_y: (optional) minimum y coordinate of the canvas
        :param max_x: (optional) maximum x coordinate of the canvas
        :param max_y: (optional) maximum y coordinate of the canvas
        """
        ret = self.line_ending.join(self.rows(min_x, min_y, max_x, max_y))

        if IS_PY3:
            return ret

        return ret.encode('utf-8')


def line(x1, y1, x2, y2):
    """Returns the coords of the line between (x1, y1), (x2, y2)

    :param x1: x coordinate of the startpoint
    :param y1: y coordinate of the startpoint
    :param x2: x coordinate of the endpoint
    :param y2: y coordinate of the endpoint
    """

    x1 = normalize(x1)
    y1 = normalize(y1)
    x2 = normalize(x2)
    y2 = normalize(y2)

    xdiff = max(x1, x2) - min(x1, x2)
    ydiff = max(y1, y2) - min(y1, y2)
    xdir = 1 if x1 <= x2 else -1
    ydir = 1 if y1 <= y2 else -1

    r = max(xdiff, ydiff)

    for i in range(r+1):
        x = x1
        y = y1

        if ydiff:
            y += (float(i) * ydiff) / r * ydir
        if xdiff:
            x += (float(i) * xdiff) / r * xdir

        yield (x, y)


def polygon(center_x=0, center_y=0, sides=4, radius=4):
    degree = float(360) / sides

    for n in range(sides):
        a = n * degree
        b = (n + 1) * degree
        x1 = (center_x + math.cos(math.radians(a))) * (radius + 1) / 2
        y1 = (center_y + math.sin(math.radians(a))) * (radius + 1) / 2
        x2 = (center_x + math.cos(math.radians(b))) * (radius + 1) / 2
        y2 = (center_y + math.sin(math.radians(b))) * (radius + 1) / 2

        for x, y in line(x1, y1, x2, y2):
            yield x, y


class Turtle(Canvas):
    """Turtle graphics interface
    http://en.wikipedia.org/wiki/Turtle_graphics
    """

    def __init__(self, pos_x=0, pos_y=0):
        self.pos_x = pos_x
        self.pos_y = pos_y
        self.rotation = 0
        self.brush_on = True
        super(Turtle, self).__init__()


    def up(self):
        """Pull the brush up."""
        self.brush_on = False


    def down(self):
        """Push the brush down."""
        self.brush_on = True


    def forward(self, step):
        """Move the turtle forward.

        :param step: Integer. Distance to move forward.
        """
        x = self.pos_x + math.cos(math.radians(self.rotation)) * step
        y = self.pos_y + math.sin(math.radians(self.rotation)) * step
        prev_brush_state = self.brush_on
        self.brush_on = True
        self.move(x, y)
        self.brush_on = prev_brush_state


    def move(self, x, y):
        """Move the turtle to a coordinate.

        :param x: x coordinate
        :param y: y coordinate
        """
        if self.brush_on:
            for lx, ly in line(self.pos_x, self.pos_y, x, y):
                self.set(lx, ly)

        self.pos_x = x
        self.pos_y = y


    def right(self, angle):
        """Rotate the turtle (positive direction).

        :param angle: Integer. Rotation angle in degrees.
        """
        self.rotation += angle


    def left(self, angle):
        """Rotate the turtle (negative direction).

        :param angle: Integer. Rotation angle in degrees.
        """
        self.rotation -= angle


    def back(self, step):
        """Move the turtle backwards.

        :param step: Integer. Distance to move backwards.
        """
        self.forward(-step)


    # aliases
    pu = up
    pd = down
    fd = forward
    mv = move
    rt = right
    lt = left
    bk = back


def animate(canvas, fn, delay=1./24, *args, **kwargs):
    """Animation automatition function

    :param canvas: :class:`Canvas` object
    :param fn: Callable. Frame coord generator
    :param delay: Float. Delay between frames.
    :param *args, **kwargs: optional fn parameters
    """

    # python2 unicode curses fix
    if not IS_PY3:
        import locale
        locale.setlocale(locale.LC_ALL, "")

    def animation(stdscr):

        for frame in fn(*args, **kwargs):
            for x,y in frame:
                canvas.set(x,y)

            f = canvas.frame()
            stdscr.addstr(0, 0, '{0}\n'.format(f))
            stdscr.refresh()
            if delay:
                sleep(delay)
            canvas.clear()

    curses.wrapper(animation)
