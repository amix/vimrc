"""
This is just a script for testing that the dumb TCP server actually works
correctly, for verifying that problems with tests are in Vim. Pass the
same port number given to the test server to check that it's working.
"""
import socket
import sys


def main():
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    result = sock.connect_ex(('127.0.0.1', int(sys.argv[1])))

    if result:
        sock.close()
        sys.exit("Couldn't connect to the socket!")

    data_sent = 'x' * 1024

    sock.send(data_sent)
    data_received = sock.recv(1024)

    if data_sent != data_received:
        sock.close()
        sys.exit("Data sent didn't match data received.")

    sock.close()

    print("Everything was just fine.")


if __name__ == "__main__":
    main()
