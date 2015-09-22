#!/bin/sh
youtube-dl "https://www.youtube.com/watch?v=rpMn6gP69eM" -f 141 -o chaos_in_progress.mp4
ffmpeg -n -i chaos_in_progress.mp4 chaos_in_progress.wav
sox chaos_in_progress.wav regrets.wav trim "50:18.914" "1:23.176" pad "5:48.438"
