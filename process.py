#!/usr/bin/env python3

import re
import fileinput

label_pat = re.compile(r'u([0-9]+)\[label="\1"];')
labels = dict()

for line in fileinput.input():
    line = line.rstrip()
    m = re.fullmatch(label_pat, line)
    nm = re.match(r"u([0-9]+)\s+->\s+u([0-9]+)", line)
    if m:
        labels[f"u{m.group(1)}"] = m.group(1)
        print(line)
    elif nm:
        new_label = f"{nm.group(1)},{nm.group(2)}"
        print(re.sub(r"\(\)", new_label, line))
    else:
        print(line)



