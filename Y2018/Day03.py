import numpy as np
import re


# [x1, y1, x2, y2]
def get_box(line):
    matches = re.search(r'(\d+),(\d+):\s+(\d+)x(\d+)', line).groups()
    m = [int(x) for x in matches]
    return [m[0], m[1], m[0] + m[2], m[1] + m[3]]


def claim(box, fabric):
    for patch in fabric[box[1]:box[3], box[0]:box[2]]:
        patch += 1


def solve1(boxes, fabric):
    for box in boxes:
        claim(box, fabric)

    two_claims = (fabric >= 2)
    print(two_claims.sum())


def solve2(boxes, fabric):
    for i, box in enumerate(boxes):
        patches = fabric[box[1]:box[3], box[0]:box[2]]
        if (patches == 1).all():
            print(i + 1)

with open('03.txt') as f:
    boxes = [get_box(line) for line in f.readlines()]

    rightmost = max([box[2] for box in boxes])
    bottommost = max([box[3] for box in boxes])
    fabric = np.zeros((bottommost, rightmost), dtype=int)

    solve1(boxes, fabric)
    solve2(boxes, fabric)
