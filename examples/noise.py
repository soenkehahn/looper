#!/usr/bin/env python

import random

volume = 0.05

for i in range(0, 44100):
    randomSample = (random.random() * 2.0) - 1.0
    print(randomSample * volume)
