#!/bin/bash
set -e
set -u

rm -f video.avi video.mp4
youtube-dl "https://www.youtube.com/watch?v=L_XJ_s5IsQc" -f 22 -o video.mp4
ffmpeg -itsoffset 2.678 -i video.mp4 -an -vf "scale=640:360" -qscale:v 5 video.avi
rm -f video.mp4
