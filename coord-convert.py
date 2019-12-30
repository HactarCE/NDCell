#!/usr/bin/env python

INPUT = '''\
........................O
......................O.O
............OO......OO............OO
...........O...O....OO............OO
OO........O.....O...OO
OO........O...O.OO....O.O
..........O.....O.......O
...........O...O
............OO
'''

y = 0
for line in INPUT.splitlines():
    x = 0
    for ch in line:
        if ch == 'O':
            print(x, y)
        x += 1
    y += 1
