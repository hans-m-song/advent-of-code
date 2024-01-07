#!/usr/bin/env python
# https://adventofcode.com/2023/day/1

import sys
import re

total = 0
for line in sys.stdin:
    line = line[:-1]
    chars = re.sub(r"[^0-9]", "", line)
    if len(chars) < 1:
        continue
    
    raw = chars[0]
    raw += chars[-1]
    
    print(f"{raw} - {line}")
    total += int(raw)

print(total)
