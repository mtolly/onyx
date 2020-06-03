#!/usr/bin/env ruby

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

filesRoot = ARGV[0]

$cardFadeIn = 0.5
$cardHold = 3
$cardFadeOut = 0.5

videos = [
  {
    # one for antonio
    songLength: 9 * 60 + 6.084,
    audio: "#{filesRoot}/audio/oneforantonio-temp.wav", # TODO replace audio
    card: "#{filesRoot}/cards/one-for-antonio.png",
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
    card: "#{filesRoot}/cards/sequence-start.png",
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
    card: "#{filesRoot}/cards/work-shit-out.png",
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
  {
    # blast off
    songLength: 5 * 60 + 34.807,
    audio: "#{filesRoot}/audio/blastoff.wav",
    card: "#{filesRoot}/cards/blast-off.png",
    sources: [
      {
        source: "#{filesRoot}/backgrounds/Blast Off stretched.mkv",
        mask: nil,
        startTime: 0,
        position: 'stretch'
      },
      {
        source: "#{filesRoot}/recorded/Blast Off CH.mkv",
        mask: "#{filesRoot}/masks/ch-3-tracks-only12-fade-bottom.png",
        startTime: 3.703,
        position: {x: 326, y: -556},
        size: {x: 1920, y: 1080},
      },
      {
        source: "#{filesRoot}/recorded/Blast Off RB.avi",
        mask: "#{filesRoot}/masks/rb-3-tracks.png",
        startTime: 5.138,
        position: {x: 0, y: 4},
        size: {x: 1920, y: 1076},
      },
    ],
  },
  {
    # the taste of filth
    songLength: 11 * 60 + 35.801,
    audio: "#{filesRoot}/audio/thetasteoffilth.wav", # TODO audio peaks a bit
    card: "#{filesRoot}/cards/the-taste-of-filth.png",
    sources: [
      # TODO find a background?
      {
        source: "#{filesRoot}/recorded/The Taste of Filth CH.mkv",
        mask: "#{filesRoot}/masks/ch-3-tracks-only12-fade-bottom.png",
        startTime: 5.446,
        position: {x: 326, y: -556},
        size: {x: 1920, y: 1080},
      },
      {
        source: "#{filesRoot}/recorded/The Taste of Filth RB.avi",
        mask: "#{filesRoot}/masks/rb-3-tracks.png",
        startTime: 4.870,
        position: {x: 0, y: 4},
        size: {x: 1920, y: 1076},
      },
    ],
  },
  {
    # tank
    songLength: 3 * 60 + 33.388,
    audio: "#{filesRoot}/audio/tank.wav",
    card: "#{filesRoot}/cards/tank.png",
    sources: [
      # TODO record and add venue from atupo
      {
        source: "#{filesRoot}/recorded/Tank RB.avi",
        mask: "#{filesRoot}/masks/rb-4-tracks-solovox.png", # TODO fade in/out vox
        startTime: 4.140,
        position: {x: 0, y: 4},
        size: {x: 1920, y: 1076},
      },
    ],
  },
  {
    # Caravan
    songLength: 9 * 60 + 18.167,
    audio: "#{filesRoot}/audio/caravan-with-gap.wav", # gap added from 9:06.164 to 9:15.867
    card: "#{filesRoot}/cards/caravan.png",
    sources: [
      {
        source: "#{filesRoot}/backgrounds/Caravan-final.mkv",
        mask: nil,
        startTime: 0,
        position: {x: 0, y: -142}, # move up to cut the top letterbox bar
        size: {x: 1920, y: 1080},
      },
      {
        source: "#{filesRoot}/recorded/Caravan RB with gap.mkv", # gap added from 9:05.453 to 9:15.156
        mask: "#{filesRoot}/masks/rb-4-tracks-only2.png",
        # mask: [
        #   [0, "#{filesRoot}/masks/rb-4-tracks-only2.png"],
        #   [22.291, :fade],
        #   [23.167, "#{filesRoot}/masks/rb-4-tracks-only23.png"],
        #   [35.987, :fade],
        #   [36.844, "#{filesRoot}/masks/rb-4-tracks-only234.png"],
        #   [42.864, :fade],
        #   [43.703, "#{filesRoot}/masks/rb-4-tracks.png"],
        #   [4 * 60 + 33.062, :fade],
        #   [4 * 60 + 34.002, "#{filesRoot}/masks/rb-4-tracks-only2.png"],
        #   # TODO fade to black during gap
        #   [9 * 60 + 14.948, :fade],
        #   [9 * 60 + 15.533, "#{filesRoot}/masks/rb-4-tracks.png"],
        # ],
        startTime: 0, # original footage started at 5.000
        position: {x: 0, y: 4},
        size: {x: 1920, y: 1076},
      },
    ],
  },
  {
    # Alien Lair
    songLength: 3 * 60 + 10.373,
    audio: "#{filesRoot}/audio/alienlair.wav",
    card: "#{filesRoot}/cards/alien-lair.png",
    sources: [
      # TODO find a background?
      {
        source: "#{filesRoot}/recorded/Alien Lair RB.avi",
        mask: "#{filesRoot}/masks/rb-2-tracks.png",
        startTime: 2.476,
        position: {x: 0, y: -500},
        size: {x: 1920, y: 1076},
      },
      {
        source: "#{filesRoot}/recorded/Alien Lair CH.mkv",
        mask: "#{filesRoot}/masks/ch-4-tracks.png",
        startTime: 6.111,
        position: {x: 0, y: 0},
        size: {x: 1920, y: 1080},
      },
    ],
  },
]

class Node
  def initialize(inputs, filter)
    @inputs = inputs
    @filter = filter
  end

  def generate_child(next_index)
    lines = []
    input_names = []
    inputs = (@inputs.respond_to?(:each) ? @inputs : [@inputs])
    inputs.each do |input|
      if input.respond_to? :generate_child
        created_name, created_index, new_lines = input.generate_child(next_index)
        lines += new_lines
        next_index = created_index + 1
        input_names << created_name
      else
        input_names << input # raw number/string
      end
    end
    my_node = "node#{next_index}"
    lines << "#{input_names.map { |x| "[#{x}]" }.join('')} #{@filter} [#{my_node}]"
    return [my_node, next_index, lines]
  end

  def generate_root
    created_name, created_index, lines = self.generate_child(0)
    lines << "[#{created_name}] null"
    return lines
  end
end

def buildCommand(video, out)
  cmd = ['ffmpeg']

  sources = []
  sources << ['-loop', '1', '-i', video[:card]]
  sources << ['-i', video[:audio]]
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
  card = Node.new([0], "format=rgba, scale=1920x1080,
    fade=t=out:st=#{$cardFadeIn + $cardHold}:d=#{$cardFadeOut}:alpha=1,
    fade=t=in:st=0:d=#{$cardFadeIn}:color=black")
  stack = Node.new([], 'color=size=1920x1080:color=black:rate=60')
  video[:sources].each_with_index do |src, i|
    inputs = sourceMap[i]
    size = '1920x1080'
    if src[:position] != 'stretch'
      size = "#{src[:size][:x]}x#{src[:size][:y]}"
    end
    node = Node.new(["#{inputs[0]}:v"], "fps=60, scale=#{size}")
    startTime = $cardFadeIn + $cardHold - src[:startTime]
    node = Node.new([node], "setpts=PTS-STARTPTS+#{startTime}/TB")
    if src[:mask]
      node = Node.new([node, "#{inputs[1]}:v"], "alphamerge")
    end
    if src[:scale]
      scale = "#{src[:scale][:x]}x#{src[:scale][:y]}"
      node = Node.new([node], "scale=#{scale}")
    end
    if src[:position] == 'stretch'
      command = 'overlay'
    else
      command = "overlay=x=#{src[:position][:x]}:y=#{src[:position][:y]}"
    end
    stack = Node.new([stack, node], command)
  end
  filter = Node.new([stack, card], 'overlay').generate_root
  msdelay = (($cardFadeIn + $cardHold) * 1000).floor
  filter << "[1:a] adelay=delays=#{msdelay}|#{msdelay}"
  cmd << filter.join(";\n")

  cmd += %W{
    -c:v libx264
    -r 60
    -t #{$cardFadeIn + $cardHold + video[:songLength]}
    #{out}
  }

  p sourceMap
  print cmd
  system(*(cmd.map(&:to_s)))
end

puts buildCommand(videos[ARGV[1].to_i], ARGV[2])
