#!/bin/bash
set -e
set -u

# Downloads the music video for use as Phase Shift background.

rm -f video.avi video.webm
youtube-dl "https://www.youtube.com/watch?v=dRBP1rpE5y8" -f 43 -o video.webm
# use "ffplay -i video.webm -vf cropdetect" to get the below crop values
ffmpeg -itsoffset 5.195 -i video.webm -an -vf "crop=480:272:0:44" -qscale:v 5 video.avi
rm -f video.webm

# put in ps .ini:
# video = video.avi
