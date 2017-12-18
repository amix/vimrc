# example:
# $  PYTHONPATH=`pwd` python examples/image2term.py http://fc00.deviantart.net/fs71/f/2011/310/5/a/giant_nyan_cat_by_daieny-d4fc8u1.png -t 100 -r 0.01

try:
    from PIL import Image
except:
    from sys import stderr
    stderr.write('[E] PIL not installed\n')
    exit(1)
from drawille import Canvas
from StringIO import StringIO
import urllib2


def getTerminalSize():
    import os
    env = os.environ

    def ioctl_GWINSZ(fd):
        import fcntl
        import termios
        import struct
        cr = struct.unpack('hh', fcntl.ioctl(fd, termios.TIOCGWINSZ, '1234'))
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


def image2term(image, threshold=128, ratio=None, invert=False):
    if image.startswith('http://') or image.startswith('https://'):
        i = Image.open(StringIO(urllib2.urlopen(image).read())).convert('L')
    else:
        i = Image.open(open(image)).convert('L')
    w, h = i.size
    if ratio:
        w = int(w * ratio)
        h = int(h * ratio)
        i = i.resize((w, h), Image.ANTIALIAS)
    else:
        tw = getTerminalSize()[0]
        tw *= 2
        if tw < w:
            ratio = tw / float(w)
            w = tw
            h = int(h * ratio)
            i = i.resize((w, h), Image.ANTIALIAS)
    can = Canvas()
    x = y = 0

    try:
         i_converted = i.tobytes()
    except AttributeError:
         i_converted = i.tostring()

    for pix in i_converted:
        if invert:
            if ord(pix) > threshold:
                can.set(x, y)
        else:
            if ord(pix) < threshold:
                can.set(x, y)
        x += 1
        if x >= w:
            y += 1
            x = 0
    return can.frame(0, 0)


def argparser():
    import argparse
    from sys import stdout
    argp = argparse.ArgumentParser(description='drawille - image to terminal example script')
    argp.add_argument('-o', '--output'
                     ,help      = 'Output file - default is STDOUT'
                     ,metavar   = 'FILE'
                     ,default   = stdout
                     ,type      = argparse.FileType('w')
                     )
    argp.add_argument('-r', '--ratio'
                     ,help      = 'Image resize ratio'
                     ,default   = None
                     ,action    = 'store'
                     ,type      = float
                     ,metavar   = 'N'
                     )
    argp.add_argument('-t', '--threshold'
                     ,help      = 'Color threshold'
                     ,default   = 128
                     ,action    = 'store'
                     ,type      = int
                     ,metavar   = 'N'
                     )
    argp.add_argument('-i', '--invert'
                     ,help      = 'Invert colors'
                     ,default   = False
                     ,action    = 'store_true'
                     )
    argp.add_argument('image'
                     ,metavar   = 'FILE'
                     ,help      = 'Image file path/url'
                     )
    return vars(argp.parse_args())


def __main__():
    args = argparser()
    args['output'].write(image2term(args['image'], args['threshold'], args['ratio'], args['invert']))
    args['output'].write('\n')


if __name__ == '__main__':
    __main__()
