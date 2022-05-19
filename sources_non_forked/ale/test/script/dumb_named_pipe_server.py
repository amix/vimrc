"""
This Python script creates a named pipe server that does nothing but send its input
back to the client that connects to it. Only one argument must be given, the path
of a named pipe to bind to.
"""
import os
import socket
import sys


def main():
    if len(sys.argv) < 2:
        sys.exit('You must specify a filepath')

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    if os.path.exists(sys.argv[1]):
        os.remove(sys.argv[1])
    sock.bind(sys.argv[1])
    sock.listen(0)

    pid = os.fork()

    if pid:
        print(pid)
        sys.exit()

    while True:
        connection = sock.accept()[0]
        connection.settimeout(5)

        while True:
            try:
                connection.send(connection.recv(1024))
            except socket.timeout:
                break

        connection.close()


if __name__ == "__main__":
    main()
