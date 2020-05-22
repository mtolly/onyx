#!/usr/bin/env ruby

# video start times

# alien lair CH = 6.111
# blast off CH = 3.703
# one for antonio CH = 3.801
# sequence start CH = 4.143
# the taste of filth CH = 5.446
# work shit out CH = 5.190

# alien lair RB = 2.476
# blast off RB = 5.138
# caravan RB = 5.000
# one for antonio RB = 5.404
# sequence start RB = 3.670
# tank RB = 4.140
# the taste of filth RB = 4.870
# work shit out RB = 2.270

# [end] times

# one for antonio = 9:06.084
# tank = 3:33.388
# work shit out = 7:53.379
# blast off = 5:34.807
# alien lair = 3:10.373
# the taste of filth = 11:35.801
# sequence start = 1:37.440
# caravan = 9:18.167

# > alien lair (shnabubula)
#   - maybe overlay on https://www.youtube.com/watch?v=EMPeH_tkeZ0 (time to final hit at 2:49)
#   - top: RB pro keys, pro drums
#   - bottom: CH gtr/keys/bass/rhythm (gtr on L, rhythm on R)
# > one for antonio (chick corea + antonio sanchez)
#   - overlay on https://www.youtube.com/watch?v=ZMHyRjIGrOc (pad by 1.534)
#   - CH charts on top, RB pro on bottom
# > caravan (whiplash ost)
#   - overlay on movie footage?
#     https://www.youtube.com/watch?v=ZZY-Ytrw2co
#     https://www.youtube.com/watch?v=2TAfvMn8_EQ
#   - switch to just drums in second half
#   - just RB charts, with pro bass
# > sequence start (sungazer)
#   - overlay tracks on https://www.youtube.com/watch?v=Ta4TsfGwMBI
#   - CH charts on top, RB pro on bottom
# > blast off (trioscapes)
#   - just CH charts on top, RB pro on bottom
# > tank! (seatbelts)
#   - maybe https://www.youtube.com/watch?v=2VsgkIE-RHg
#     (cowboy bebop intro not long enough)
#   - or, ask atupo to do a venue!
#   - just RB charts, with pro bass
#   - hide vocals after they're done
# > work shit out (dirty loops)
#   - https://www.youtube.com/watch?v=r_GTgpdoCh0
#   - vocals in middle (maybe shrink note display vertically), top: CH keys/drums/bass, bottom: RB pro keys/drums/bass
#   - hide vocals after they're done
# > the taste of filth (louis de mieulle)
#   - just CH charts on top, RB pro on bottom

# one for antonio:
# rb-3-tracks 4px down
# ch-2-tracks-only12 326px right 556px up

filesRoot = ARGV[0]

$cardFadeIn = 0.5
$cardHold = 3
$cardFadeOut = 0.5

videos = [
  {
    # one for antonio
    songLength: 9 * 60 + 6.084,
    audio: "#{filesRoot}/audio/oneforantonio-temp.wav",
    card: "#{filesRoot}/cards/dummy.png",
    sources: [
      {
        source: "#{filesRoot}/backgrounds/Antonio Sanchez - One for Antonio-ZMHyRjIGrOc.mp4",
        mask: nil,
        startTime: -1.534,
        position: 'stretch',
      },
      {
        source: "#{filesRoot}/recorded/One for Antonio CH.mkv",
        mask: "#{filesRoot}/masks/ch-3-tracks-only12-fade-bottom.png",
        startTime: 3.801,
        position: {x: 326, y: -556},
        size: {x: 1920, y: 1080},
      },
      {
        source: "#{filesRoot}/recorded/One for Antonio RB.avi",
        mask: "#{filesRoot}/masks/rb-3-tracks.png",
        startTime: 5.404,
        position: {x: 0, y: 4},
        size: {x: 1920, y: 1076},
      },
    ],
  },
  {
    # sequence start
    songLength: 1 * 60 + 37.440,
    audio: "#{filesRoot}/audio/sequencestart.wav",
    card: "#{filesRoot}/cards/dummy.png",
    sources: [
      {
        source: "#{filesRoot}/backgrounds/Sungazer - 'Sequence Start'-Ta4TsfGwMBI.mp4",
        mask: nil,
        startTime: -3.040,
        position: 'stretch',
      },
      {
        source: "#{filesRoot}/recorded/Sequence Start CH.mkv",
        mask: "#{filesRoot}/masks/ch-3-tracks-only12-fade-bottom.png",
        startTime: 4.143,
        position: {x: 326, y: -556},
        size: {x: 1920, y: 1080},
      },
      {
        source: "#{filesRoot}/recorded/Sequence Start RB.avi",
        mask: "#{filesRoot}/masks/rb-3-tracks.png",
        startTime: 3.670,
        position: {x: 0, y: 4},
        size: {x: 1920, y: 1076},
      },
    ],
  },
  {
    # work shit out
    songLength: 7 * 60 + 53.379,
    audio: "#{filesRoot}/audio/workshitout.wav",
    card: "#{filesRoot}/cards/dummy.png",
    sources: [
      {
        source: "#{filesRoot}/backgrounds/Dirty Loops - Work Shit Out-r_GTgpdoCh0.mp4",
        mask: nil,
        startTime: -0.804,
        position: 'stretch',
      },
      {
        source: "#{filesRoot}/recorded/Work Shit Out CH.mkv",
        mask: "#{filesRoot}/masks/ch-3-tracks-only12-fade-bottom.png",
        startTime: 5.190,
        size: {x: 1920, y: 1080},
        scale: {x: 1422, y: 800},
        position: {x: 489, y: -421},
      },
      {
        source: "#{filesRoot}/recorded/Work Shit Out RB.avi",
        mask: "#{filesRoot}/masks/rb-3-tracks.png",
        startTime: 2.270,
        size: {x: 1920, y: 1076},
        scale: {x: 1428, y: 800},
        position: {x: 246, y: 280},
      },
      {
        source: "#{filesRoot}/recorded/Work Shit Out RB.avi",
        mask: "#{filesRoot}/masks/rb-harmonies.png", # TODO fade out after vox is done
        startTime: 2.270,
        size: {x: 1920, y: 1076},
        position: {x: 0, y: 370},
      },
    ],
  },
]

def buildCommand(video, out)
  cmd = ['ffmpeg']

  sources = []
  sources << ['-loop', '1', '-i', video[:card]]
  sources << ['-itsoffset', $cardFadeIn + $cardHold, '-i', video[:audio]]
  sourceMap = []
  video[:sources].each do |src|
    newSources = [sources.length]
    sources << ['-i', src[:source]]
    if src[:mask]
      newSources << sources.length
      sources << ['-loop', '1', '-i', src[:mask]]
    end
    sourceMap << newSources
  end
  sources.each { |src| cmd += src }

  cmd += ['-filter_complex']
  filter = []
  filter << %{
    [0] format=rgba, scale=1920x1080,
      fade=t=out:st=#{$cardFadeIn + $cardHold}:d=#{$cardFadeOut}:alpha=1,
      fade=t=in:st=0:d=#{$cardFadeIn}:color=black [card]
  }
  stackIndex = 0
  video[:sources].each_with_index do |src, i|
    inputs = sourceMap[i]
    size = '1920x1080'
    if src[:position] != 'stretch'
      size = "#{src[:size][:x]}x#{src[:size][:y]}"
    end
    filter << "[#{inputs[0]}:v] fps=60, scale=#{size} [source#{i}]"
    result = 'source'
    startTime = $cardFadeIn + $cardHold - src[:startTime]
    if startTime < 0
      filter << "[#{result}#{i}] trim=start=#{startTime.abs}, setpts=PTS-STARTPTS [positioned#{i}]"
      result = 'positioned'
    elsif startTime > 0
      filter << "color=size=#{size}:color=black:rate=60, trim=end=#{startTime} [black#{i}]"
      filter << "[black#{i}][#{result}#{i}] concat [positioned#{i}]"
      result = 'positioned'
    end
    if src[:mask]
      filter << "[#{result}#{i}][#{inputs[1]}:v] alphamerge [masked#{i}]"
      result = 'masked'
    end
    if src[:scale]
      scale = "#{src[:scale][:x]}x#{src[:scale][:y]}"
      filter << "[#{result}#{i}] scale=#{scale} [scaled#{i}]"
      result = 'scaled'
    end
    if stackIndex == 0
      if src[:position] == 'stretch'
        filter << "[#{result}#{i}] null [stack#{stackIndex}]"
      else
        # TODO support
        putStrLn "First source position needs to be stretch"
        exit 1
      end
    else
      if src[:position] == 'stretch'
        command = 'overlay'
      else
        command = "overlay=x=#{src[:position][:x]}:y=#{src[:position][:y]}"
      end
      filter << "[stack#{stackIndex - 1}][#{result}#{i}] #{command} [stack#{stackIndex}]"
    end
    stackIndex += 1
  end
  filter << "[stack#{stackIndex - 1}][card] overlay"
  cmd << filter.map(&:strip).join(";\n")

  cmd += %W{
    -c:v libx264
    -r 60
    -map 1:a
    -t #{$cardFadeIn + $cardHold + video[:songLength]}
    #{out}
  }

  p sourceMap
  print cmd
  system(*(cmd.map(&:to_s)))
end

puts buildCommand(videos[ARGV[1].to_i], ARGV[2])
