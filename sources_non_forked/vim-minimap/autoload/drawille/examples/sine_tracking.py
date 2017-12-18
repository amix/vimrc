from __future__ import print_function
from drawille import Canvas, line, animate
import math

def __main__():
    i = 0
    height = 40

    while True:
        frame = []

        frame.extend([coords for coords in
                      line(0,
                           height,
                           180,
                           math.sin(math.radians(i)) * height + height)])

        frame.extend([(x/2, height + math.sin(math.radians(x+i)) * height)
                      for x in range(0, 360, 2)])

        yield frame

        i += 2



if __name__ == '__main__':
    animate(Canvas(), __main__, 1./60)
