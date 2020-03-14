#!/usr/bin/env python

import math
import sys

frequency = 440
volume = 0.05
tau = 2.0 * math.pi

phase = 0
for i in range(0, 44100):
    phase += frequency * tau / 44100
    while (phase >= tau):
        phase -= tau
    sample = math.sin(phase)
    print(sample * volume)
