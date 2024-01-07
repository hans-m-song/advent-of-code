#!/usr/bin/env python

import sys
import re
from webbrowser import get

mappings = {
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9,
}


def get_int(input: str) -> int:
  """get_int returns the first integer in the input string, symbol or english"""
  if input[0].isdigit():
    return int(input[0])

  for key, value in mappings.items():
    if input.startswith(key):
      return value

  return 0


def get_first(input: str) -> int:
  """get_first searches input for the first instance of a non-zero integer"""
  for i in range(len(input)):
    val = get_int(input[i:])
    if val > 0:
      return val
  return 0


def get_last(input: str) -> int:
  """get_last searches input for the last instance of a non-zero integer"""
  for i in range(len(input) - 1, -1, -1):
    val = get_int(input[i:])
    if val > 0:
      return val
  return 0


max_length = 0
total = 0
for line in sys.stdin:
  line = line[:-1]
  max_length = max(max_length, len(line))

  first = get_first(line)
  last = get_last(line)
  raw = f"{first}{last}"

  padded = line.ljust(51, " ")
  print(f"{raw} - {padded}")
  total += int(raw)

print(max_length, total)
