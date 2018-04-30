`loopnaut` is a tool that plays a single audio file in a loop. It connects to
[JACK](http://jackaudio.org/) as a client, loads the given file and plays it in
a loop. In case the given file is written to, `loopnaut` will reload the file
and switch over to the new file contents gapless after the current loop is
finished.

I use it for a form of livecoding where I have a program that writes an audio
file and I run that program over and over again while modifying it. While
`loopnaut` is playing the file.
