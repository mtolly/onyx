#!/usr/bin/env ruby

song = ARGV[0]
timeCH = ARGV[1].to_f
timeRB = ARGV[2].to_f

cmd = %W{
  ffmpeg
  -ss #{timeRB - (6.795 - 4.812)} -i #{song}_pro.avi
  -ss #{timeCH - (3.553 - 3.594)} -i #{song}_ch.flv
  -loop 1 -i #{song}_title.png
  -itsoffset 2.000 -i #{song}.ogg
  -filter_complex
}

cmd << %{
    [0:v] fps=60, scale=1280x720 [pro];
    [1:v] fps=60, scale=1280x720 [ch];
    [2] format=rgba, scale=1280x720, fade=t=out:st=2:d=0.5:alpha=1 [over];
    [pro][ch] overlay=shortest=1:y=-360 [vids];
    [vids][over] overlay
}

cmd += %W{ -c:v libx264 -r 60 #{song}_output.mkv }

system(*cmd)
