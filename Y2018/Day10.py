from PIL import Image
import numpy as np
import re


class Point:
    def __init__(self, x, y, dx, dy):
        self.x = x
        self.y = y
        self.dx = dx
        self.dy = dy

    def __repr__(self):
        return '({}, {})<{}, {}>'.format(self.x, self.y, self.dx, self.dy)

    def point_at(self, time):
        return self.x + time * self.dx, self.y + time * self.dy


def bounding_box(points):
    xs, ys = zip(*points)
    minX, minY, maxX, maxY = min(xs), min(ys), max(xs), max(ys)
    return maxX - minX, maxY - minY


def get_point(line):
    regex = re.compile(r'position=<(\s*-?\d+),(\s*-?\d+)> velocity=<(\s*-?\d+),(\s*-?\d+)>')
    m = [int(x) for x in regex.match(line).groups()]
    return Point(m[0], m[1], m[2], m[3])


with open('10.txt') as f:
    points = [get_point(line) for line in f.readlines()]
    while True:
        try:
            time = int(input())  # Answer: 10659
            t_points = [p.point_at(time) for p in points]
            (bx, by) = bounding_box(t_points)
            print('Image size: {}, {}'.format(bx, by))
            image = np.zeros((bx + 1, by + 1))
            for (x, y) in t_points:
                image[x][y] = 1
            with open('out.png', 'wb') as f_out:
                Image.fromarray(image * 255).convert('RGB').save(f_out)
                print('"out.png" created.')
        except Exception as e:
            print(e)
