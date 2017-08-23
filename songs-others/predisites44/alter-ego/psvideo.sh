#!/bin/bash
set -e
set -u

# Downloads the music video for use as Phase Shift background.

rm -f video.avi video.mp4
youtube-dl "https://www.youtube.com/watch?v=Zae4Vo6Mx8I" -f 18 -o video.mp4
ffmpeg -ss 3.179 -i video.mp4 -an -qscale:v 5 video.avi
rm -f video.mp4

# put in ps .ini:
# video = video.avi
