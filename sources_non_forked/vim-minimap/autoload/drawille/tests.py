# -*- coding: utf-8 -*-
from drawille import Canvas, line, Turtle
from unittest import TestCase, main


class CanvasTestCase(TestCase):


    def test_set(self):
        c = Canvas()
        c.set(0, 0)
        self.assertTrue(0 in c.chars and 0 in c.chars[0])


    def test_unset_empty(self):
        c = Canvas()
        c.set(1, 1)
        c.unset(1, 1)
        self.assertEqual(len(c.chars), 0)


    def test_unset_nonempty(self):
        c = Canvas()
        c.set(0, 0)
        c.set(0, 1)
        c.unset(0, 1)
        self.assertEqual(c.chars[0][0], 1)


    def test_clear(self):
        c = Canvas()
        c.set(1, 1)
        c.clear()
        self.assertEqual(c.chars, dict())


    def test_toggle(self):
        c = Canvas()
        c.toggle(0, 0)
        self.assertEqual(c.chars, {0: {0: 1}})
        c.toggle(0, 0)
        self.assertEqual(c.chars, dict())


    def test_set_text(self):
        c = Canvas()
        c.set_text(0, 0, "asdf")
        self.assertEqual(c.frame(), "asdf")


    def test_frame(self):
        c = Canvas()
        self.assertEqual(c.frame(), '')
        c.set(0, 0)
        self.assertEqual(c.frame(), '⠁')


    def test_max_min_limits(self):
        c = Canvas()
        c.set(0, 0)
        self.assertEqual(c.frame(min_x=2), '')
        self.assertEqual(c.frame(max_x=0), '')


    def test_get(self):
        c = Canvas()
        self.assertEqual(c.get(0, 0), False)
        c.set(0, 0)
        self.assertEqual(c.get(0, 0), True)
        self.assertEqual(c.get(0, 1), False)
        self.assertEqual(c.get(1, 0), False)
        self.assertEqual(c.get(1, 1), False)


class LineTestCase(TestCase):


    def test_single_pixel(self):
        self.assertEqual(list(line(0, 0, 0, 0)), [(0, 0)])


    def test_row(self):
        self.assertEqual(list(line(0, 0, 1, 0)), [(0, 0), (1, 0)])


    def test_column(self):
        self.assertEqual(list(line(0, 0, 0, 1)), [(0, 0), (0, 1)])


    def test_diagonal(self):
        self.assertEqual(list(line(0, 0, 1, 1)), [(0, 0), (1, 1)])


class TurtleTestCase(TestCase):


    def test_position(self):
        t = Turtle()
        self.assertEqual(t.pos_x, 0)
        self.assertEqual(t.pos_y, 0)
        t.move(1, 1)
        self.assertEqual(t.pos_x, 1)
        self.assertEqual(t.pos_y, 1)


    def test_rotation(self):
        t = Turtle()
        self.assertEqual(t.rotation, 0)
        t.right(30)
        self.assertEqual(t.rotation, 30)
        t.left(30)
        self.assertEqual(t.rotation, 0)


    def test_brush(self):
        t = Turtle()
        self.assertFalse(t.get(t.pos_x, t.pos_y))
        t.forward(1)
        self.assertTrue(t.get(0, 0))
        self.assertTrue(t.get(t.pos_x, t.pos_y))
        t.up()
        t.move(2, 0)
        self.assertFalse(t.get(t.pos_x, t.pos_y))
        t.down()
        t.move(3, 0)
        self.assertTrue(t.get(t.pos_x, t.pos_y))


if __name__ == '__main__':
    main()
