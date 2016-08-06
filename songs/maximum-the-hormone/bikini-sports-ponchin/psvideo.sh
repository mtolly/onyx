#!/bin/bash
set -e
set -u

# Downloads the music video for use as Phase Shift background.

rm -f video.avi video.webm
youtube-dl "https://www.youtube.com/watch?v=Xbvz8v0fHWA" -f 43 -o video.webm
ffmpeg -ss 3.559 -i video.webm -an -qscale:v 5 video.avi
rm -f video.webm

# put in ps .ini:
# video = video.avi
