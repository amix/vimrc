# -*- coding: utf-8 -*-
import curses
from drawille import Canvas, line
from time import sleep
from thread import start_new_thread
from Queue import Queue
import locale
from random import randint

locale.setlocale(locale.LC_ALL,"")

stdscr = curses.initscr()
stdscr.refresh()

keys = Queue()

speed = 0.0
fps = 20
frame_no = 0
score = 0
delta = frame_no / fps

height = 100
width = 100
position = height / 2

bird_map = [
#1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
[0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0], #1
[0,0,0,0,0,1,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0], #2
[0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0], #3
[0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,1,0,1,0,0,0], #4
[0,0,1,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0], #5
[0,1,1,1,1,1,1,1,1,0,1,0,0,0,0,1,0,0,1,0,0], #6
[1,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0], #7
[1,0,0,0,0,0,0,1,0,0,0,0,1,1,1,1,1,1,1,1,0], #8
[1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1], #9
[1,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1,1,1,0], #0
[0,1,1,1,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0], #1
[0,0,0,0,0,0,1,0,0,0,0,0,1,1,1,1,1,1,1,0,0], #2
[0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0], #3
]
bird = []
for y, row in enumerate(bird_map):
    for x,col in enumerate(row):
        if col:
            bird.append((x, y))

def read_keys(stdscr):
    while 1:
        c = stdscr.getch()
        keys.put(c)


class Bar():


    def __init__(self, bar_width, cap_height=4, space=3*13):
        self.height = randint(cap_height+space+1, height-1-cap_height)
        self.width = bar_width
        self.cap_height = cap_height
        self.x = width - bar_width - 1
        self.space = space


    def draw(self):
        for x,y in line(self.x,
                        self.height,
                        self.x+self.width,
                        self.height):
            yield x, y
        for x,y in line(self.x,
                        self.height,
                        self.x,
                        self.height+self.cap_height):
            yield x, y
        for x,y in line(self.x+self.width,
                        self.height,
                        x+self.width,
                        self.height+self.cap_height):
            yield x, y
        for x,y in line(self.x,
                        self.height+self.cap_height,
                        self.x+2,
                        self.height+self.cap_height):
            yield x, y
        for x,y in line(self.x+self.width-2,
                        self.height+self.cap_height,
                        self.x+self.width,
                        self.height+self.cap_height):
            yield x, y
        for x,y in line(self.x+2,
                        self.height+self.cap_height,
                        self.x+2,
                        height):
            yield x, y
        for x,y in line(self.x+self.width-2,
                        self.height+self.cap_height,
                        self.x+self.width-2,
                        height):
            yield x, y

        for x,y in line(self.x,
                        self.height-self.space,
                        self.x+self.width,
                        self.height-self.space):
            yield x, y
        for x,y in line(self.x,
                        self.height-self.space,
                        self.x,
                        self.height-self.cap_height-self.space):
            yield x, y
        for x,y in line(self.x+self.width,
                        self.height-self.space,
                        x+self.width,
                        self.height-self.cap_height-self.space):
            yield x, y
        for x,y in line(self.x,
                        self.height-self.cap_height-self.space,
                        self.x+2,
                        self.height-self.cap_height-self.space):
            yield x, y
        for x,y in line(self.x+self.width-2,
                        self.height-self.cap_height-self.space,
                        self.x+self.width,
                        self.height-self.cap_height-self.space):
            yield x, y
        for x,y in line(self.x+2,
                        self.height-self.cap_height-self.space,
                        self.x+2,
                        0):
            yield x, y
        for x,y in line(self.x+self.width-2,
                        self.height-self.cap_height-self.space,
                        self.x+self.width-2,
                        0):
            yield x, y

def check_collision(bird_pos, bar):
    # TODO more efficient collision detection
    if bar.x > 21:
        return False
    if bar.height <= bird_pos-13 and bar.height+bar.space > bird_pos:
        return False
    for bar_x, bar_y in bar.draw():
        for bird_x, bird_y in bird:
            if int(bird_x) == int(bar_x) and int(bird_y+bird_pos) == int(bar_y):
                return True
    return False

def main(stdscr):
    global frame_no, speed, position, score
    c = Canvas()
    bar_width = 16
    bars = [Bar(bar_width)]
    stdscr.refresh()

    while True:
        frame_no += 1
        for bar in bars:
            if check_collision(position, bar):
                return
        while not keys.empty():
            if keys.get() == 113:
                return
            speed = 32.0

        c.set(0,0)
        c.set(width, height)
        if frame_no % 50 == 0:
            bars.append(Bar(bar_width))
        for x,y in bird:
            c.set(x,y+position)
        for bar_index, bar in enumerate(bars):
            if bar.x < 1:
                bars.pop(bar_index)
                score += 1
            else:
                bars[bar_index].x -= 1
                for x,y in bar.draw():
                    c.set(x,y)
        f = c.frame()+'\n'
        stdscr.addstr(0, 0, f)
        stdscr.addstr(height/4+1, 0, 'score: {0}'.format(score))
        stdscr.refresh()
        c.clear()

        speed -= 2

        position -= speed/10

        if position < 0:
            position = 0
            speed = 0.0
        elif position > height-13:
            position = height-13
            speed = 0.0


        sleep(1.0/fps)


if __name__ == '__main__':
    start_new_thread(read_keys, (stdscr,))
    curses.wrapper(main)
    print('Final score: {0}'.format(score))

