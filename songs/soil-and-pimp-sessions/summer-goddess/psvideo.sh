#!/bin/bash
set -e
set -u

# Downloads the music video for use as Phase Shift background.

rm -f video.avi video.mp4
youtube-dl "https://www.youtube.com/watch?v=AQMgXPFzdg8" -f 18 -o video.mp4
ffmpeg -ss 10.367 -i video.mp4 -an -vf "crop=480:202:0:78" -qscale:v 5 video.avi
rm -f video.mp4

# put in ps .ini:
# video = video.avi
