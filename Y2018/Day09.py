import re


class Doubly:
    def __init__(self, value, prev=None, next=None):
        self.value = value
        self.prev = prev or self
        self.next = next or self

    def move(self, n):
        curr = self
        for i in range(abs(n)):
            if n < 0:
                curr = curr.prev
            else:
                curr = curr.next
        return curr

    def insert(self, v):
        prev = self.prev
        new_node = Doubly(v, prev, self)
        prev.next = new_node
        self.prev = new_node
        return new_node

    # Make sure 'del' this too
    def delete(self):
        self.prev.next = self.next
        self.next.prev = self.prev
        return self.value, self.next

def put_marble(t, c):
    return c.move(2).insert(t)

def put_marble_23(n_player, t, c, s):
    player = t % n_player
    p1 = t
    (p2, nc) = c.move(-7).delete()
    del c
    s[player] += p1 + p2
    return nc, s

def game(n_player, max_turn):
    c = Doubly(0)
    s = [0 for i in range(n_player + 1)]
    for t in range(1, max_turn + 1):
        if t % 23 != 0:
            c = put_marble(t, c)
        else:
            (c, s) = put_marble_23(n_player, t, c, s)
    return s

def solve1(n_player, turn):
    return max(game(n_player, turn))

def solve2(n_player, turn):
    return max(game(n_player, turn * 100))

with open('09.txt') as f:
    line = f.read()
    [n_player, turn] = [int(x) for x in re.search(r'(\d+)[^\d]*(\d+).*$', line).groups()]
    print(solve1(n_player, turn))
    print(solve2(n_player, turn))
