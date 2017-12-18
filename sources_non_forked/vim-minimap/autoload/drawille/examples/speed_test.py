from drawille import Canvas
from timeit import timeit

c = Canvas()

frames = 1000 * 10

sizes = ((0, 0),
         (10, 10),
         (20, 20),
         (20, 40),
         (40, 20),
         (40, 40),
         (100, 100))

for x, y in sizes:
    c.set(0, 0)

    for i in range(y):
        c.set(x, i)

    r = timeit(c.frame, number=frames)
    print('{0}x{1}\t{2}'.format(x, y, r))
    c.clear()
