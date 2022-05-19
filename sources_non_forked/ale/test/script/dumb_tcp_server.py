"""
This Python script creates a TCP server that does nothing but send its input
back to the client that connects to it. Only one argument must be given, a port
to bind to.
"""
import os
import socket
import sys


def main():
    if len(sys.argv) < 2 or not sys.argv[1].isdigit():
        sys.exit('You must specify a port number')

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(('127.0.0.1', int(sys.argv[1])))
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
