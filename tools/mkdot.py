# Format:
"""
BB0:
  %0 = alloca 4
  %1 = 0
  store %0, u32 %1
  %5 = cmp ne, %1, 0
  br-cond %5, BB2, BB1
BB1:
  %3 = 3
  store %0, u32 %3
  br  BB2
BB2:
  %6 = phi [ %1, BB0 ], [ %3, BB1 ]
  %7 = cmp ne, %6, 0
  %10 = cmp ne, %7, 0
  br-cond %10, BB4, BB3
BB3:
  %8 = 4
  store %0, u32 %8
  br  BB4
BB4:
  %13 = phi [ %8, BB3 ], [ %1, BB0 ], [ %3, BB1 ]
  ret %13
"""

import sys
from collections import defaultdict

x = defaultdict(lambda: [])

toshow = set()

with open('ir.dot', 'w') as out:
    print("digraph IR {", file=out)
    cur = None
    for line in sys.stdin:
        if line.startswith('BB'):
            cur, _, _ = line.partition(':')
            toshow.add(cur)
        elif line.startswith('  br-cond'):
            toshow.remove(cur)
            [_, l, r] = line.split(',')
            l = l.strip()
            r = r.strip()
            print(f"{cur} -> {l};", file=out)
            print(f"{cur} -> {r};", file=out)
        elif line.startswith('  br'):
            toshow.remove(cur)
            _, _, r = line.partition('B')
            r = r.strip()
            print(f"{cur} -> B{r};", file=out)

    for remain in toshow:
        print(f"{remain};" ,file=out)

    print("}", file=out)

