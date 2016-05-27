#!/bin/bash
youtube-dl "https://www.youtube.com/watch?v=fte2YrMta7g" -f 140 -o audio-youtube.m4a
ffmpeg -i audio-youtube.m4a audio-youtube.wav
rm audio-youtube.m4a
