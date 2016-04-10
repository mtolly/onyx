#!/bin/bash
youtube-dl "https://www.youtube.com/watch?v=q7Gcg6Kpq2M" -f 141 -o audio-youtube.m4a
ffmpeg -i audio-youtube.m4a audio-youtube.wav
rm audio-youtube.m4a
