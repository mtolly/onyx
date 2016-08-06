#!/bin/bash
set -e
set -u

# Downloads the music video for use as Phase Shift background.

rm -f video.avi video.mp4
youtube-dl "https://www.youtube.com/watch?v=7PFlN758Ktk" -f 22 -o video.mp4
ffmpeg -itsoffset 2.873 -i video.mp4 -an -vf "scale=640:360" -qscale:v 5 video.avi
rm -f video.mp4

# put in ps .ini:
# video = video.avi
