from drawille import Canvas, line
import curses
import math
from time import sleep
import locale

locale.setlocale(locale.LC_ALL,"")

stdscr = curses.initscr()
stdscr.refresh()

class Point3D:
    def __init__(self, x = 0, y = 0, z = 0):
        self.x, self.y, self.z = float(x), float(y), float(z)

    def rotateX(self, angle):
        """ Rotates the point around the X axis by the given angle in degrees. """
        rad = angle * math.pi / 180
        cosa = math.cos(rad)
        sina = math.sin(rad)
        y = self.y * cosa - self.z * sina
        z = self.y * sina + self.z * cosa
        return Point3D(self.x, y, z)

    def rotateY(self, angle):
        """ Rotates the point around the Y axis by the given angle in degrees. """
        rad = angle * math.pi / 180
        cosa = math.cos(rad)
        sina = math.sin(rad)
        z = self.z * cosa - self.x * sina
        x = self.z * sina + self.x * cosa
        return Point3D(x, self.y, z)

    def rotateZ(self, angle):
        """ Rotates the point around the Z axis by the given angle in degrees. """
        rad = angle * math.pi / 180
        cosa = math.cos(rad)
        sina = math.sin(rad)
        x = self.x * cosa - self.y * sina
        y = self.x * sina + self.y * cosa
        return Point3D(x, y, self.z)

    def project(self, win_width, win_height, fov, viewer_distance):
        """ Transforms this 3D point to 2D using a perspective projection. """
        factor = fov / (viewer_distance + self.z)
        x = self.x * factor + win_width / 2
        y = -self.y * factor + win_height / 2
        return Point3D(x, y, 1)


vertices = [
    Point3D(-20,20,-20),
    Point3D(20,20,-20),
    Point3D(20,-20,-20),
    Point3D(-20,-20,-20),
    Point3D(-20,20,20),
    Point3D(20,20,20),
    Point3D(20,-20,20),
    Point3D(-20,-20,20)
]

# Define the vertices that compose each of the 6 faces. These numbers are
# indices to the vertices list defined above.
faces = [(0,1,2,3),(1,5,6,2),(5,4,7,6),(4,0,3,7),(0,4,5,1),(3,2,6,7)]


def __main__(stdscr, projection=False):
    angleX, angleY, angleZ = 0, 0, 0
    c = Canvas()
    while 1:
        # Will hold transformed vertices.
        t = []

        for v in vertices:
            # Rotate the point around X axis, then around Y axis, and finally around Z axis.
            p = v.rotateX(angleX).rotateY(angleY).rotateZ(angleZ)
            if projection:
                # Transform the point from 3D to 2D
                p = p.project(50, 50, 50, 50)
             #Put the point in the list of transformed vertices
            t.append(p)

        for f in faces:
            for x,y in line(t[f[0]].x, t[f[0]].y, t[f[1]].x, t[f[1]].y):
                c.set(x,y)
            for x,y in line(t[f[1]].x, t[f[1]].y, t[f[2]].x, t[f[2]].y):
                c.set(x,y)
            for x,y in line(t[f[2]].x, t[f[2]].y, t[f[3]].x, t[f[3]].y):
                c.set(x,y)
            for x,y in line(t[f[3]].x, t[f[3]].y, t[f[0]].x, t[f[0]].y):
                c.set(x,y)

        f = c.frame(-40, -40, 80, 80)
        stdscr.addstr(0, 0, '{0}\n'.format(f))
        stdscr.refresh()

        angleX += 2
        angleY += 3
        angleZ += 5
        sleep(1.0/20)
        c.clear()

if __name__ == '__main__':
    from sys import argv
    projection = False
    if '-p' in argv:
        projection = True
    curses.wrapper(__main__, projection)
