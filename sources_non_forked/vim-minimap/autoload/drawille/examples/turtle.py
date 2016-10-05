from drawille import Turtle

t = Turtle()

for _ in range(36):
    t.right(10)
    for _ in range(36):
        t.right(10)
        t.forward(8)

print(t.frame())
