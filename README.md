`loopnaut` is a tool that allows you to easily experiment with a simple form of
livecoding for audio generaton. It plays back snippets of audio in a gapless,
infinite loop. These snippets are meant to be generated by a program that can be
modified. `loopnaut` then picks up the changes to the program, re-runs it, and
switches to the newly generated audio snippet in the next loop.

## how it works

Create an executable in a programming language of your choice that outputs
floating point numbers to `stdout`. These will be interpreted by `loopnaut` as a
sequence of audio samples in mono, in 44100 Hertz between -1.0 to +1.0. Here's
an example python program `noise.py` that generates one second of noise:

```python
#!/usr/bin/env python

import random

for i in range(0, 44100):
    print((random.random() * 2.0) - 1.0)
```

The file has to have the executable flag set (with e.g. `chmod +x noise.py`).
Then run `loopnaut`, passing in the executable file as an argument:

```bash
loopnaut noise.py
```

This will run `noise.py`, collect the samples from its `stdout` and play them
back through your audio device in an infinite loop. Once you change and save the
program, `loopnaut` will rerun it and switch to playing the new loop.

You can use this for a simple, experimental form of livecoding by modifying the
program over and over again and listening to the results.

## Installation

You can install `loopnaut` with [stack](https://haskellstack.org/):

```bash
git clone https://github.com/soenkehahn/loopnaut
cd loopnaut
stack install
```