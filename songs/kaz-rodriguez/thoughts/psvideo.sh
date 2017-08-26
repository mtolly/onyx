#!/bin/bash
set -e
set -u

# Downloads the music video for use as Phase Shift background.

rm -f video.avi video.mp4
youtube-dl "https://www.youtube.com/watch?v=bhYqiIl1Tec" -f 18 -o video.mp4
ffmpeg -itsoffset 0.493 -i video.mp4 -an -qscale:v 5 video.avi
rm -f video.mp4

# put in ps .ini:
# video = video.avi
