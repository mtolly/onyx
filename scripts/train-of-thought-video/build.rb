#!/usr/bin/env ruby

cardFadeIn = 0.5
cardHold = 3
cardFadeOut = 0.5

songs = [
  {
    insts: "../TOT-chart-videos/As I Am.avi",
    venue: "../TOT-chart-videos/As I Am VENUE.mp4",
    instsStart: 6.317,
    venueStart: 9.435,
    songLength: 7 * 60 + 53.426,
    audio: "asiam.wav",
    card: "1 As I Am.png",
    mask: "mask.png",
    pro: "../TOT-chart-videos/As I Am PRO.avi",
    ch: "../TOT-chart-videos/As I Am CH.flv",
    proStart: 6.491,
    chStart: 4.284,
  },
  {
    insts: "../TOT-chart-videos/This Dying Soul.avi",
    venue: "../TOT-chart-videos/This Dying Soul VENUE.mp4",
    instsStart: 7.427,
    venueStart: 5.373,
    songLength: 11 * 60 + 31.383,
    audio: "thisdyingsoul.wav",
    card: "2 This Dying Soul.png",
    mask: "mask.png",
    pro: "../TOT-chart-videos/This Dying Soul PRO.avi",
    ch: "../TOT-chart-videos/This Dying Soul CH.flv",
    proStart: 9.227,
    chStart: 5.450,
  },
  {
    insts: "../TOT-chart-videos/Endless Sacrifice.avi",
    venue: "../TOT-chart-videos/Endless Sacrifice VENUE.mp4",
    instsStart: 5.823,
    venueStart: 12.617,
    songLength: 11 * 60 + 29.601,
    audio: "endlesssacrifice.wav",
    card: "3 Endless Sacrifice.png",
    mask: "mask.png",
    pro: "../TOT-chart-videos/Endless Sacrifice PRO.avi",
    ch: "../TOT-chart-videos/Endless Sacrifice CH.flv",
    proStart: 7.077,
    chStart: 4.550,
  },
  {
    insts: "../TOT-chart-videos/Honor Thy Father.avi",
    venue: "../TOT-chart-videos/Honor Thy Father VENUE.mp4",
    instsStart: 5.979,
    venueStart: 14.235,
    songLength: 10 * 60 + 13.193,
    audio: "honorthyfather.wav",
    card: "4 Honor Thy Father.png",
    mask: "mask.png",
    pro: "../TOT-chart-videos/Honor Thy Father PRO.avi",
    ch: "../TOT-chart-videos/Honor Thy Father CH.flv",
    proStart: 8.023,
    chStart: 5.084,
  },
  {
    insts: "../TOT-chart-videos/Vacant.avi",
    venue: "../TOT-chart-videos/Vacant VENUE.mp4",
    instsStart: 0.875,
    venueStart: 7.579,
    songLength: 3 * 60 + 5.432,
    audio: "vacant.wav",
    card: "5 Vacant.png",
    mask: "mask-vacant.png",
    pro: "../TOT-chart-videos/Vacant PRO.avi",
    ch: "../TOT-chart-videos/Vacant CH.flv",
    proStart: 3.616,
    chStart: 4.400,
  },
  {
    insts: "../TOT-chart-videos/Stream of Consciousness.avi",
    venue: "../TOT-chart-videos/Stream of Consciousness VENUE.mp4",
    instsStart: 6.861,
    venueStart: 11.871,
    songLength: 11 * 60 + 25.138,
    audio: "streamofconsciousness.wav",
    card: "6 Stream of Consciousness.png",
    pro: "../TOT-chart-videos/Stream of Consciousness PRO.avi",
    ch: "../TOT-chart-videos/Stream of Consciousness CH.flv",
    mask: "mask-soc.png",
    proStart: 8.695,
    chStart: 5.500,
  },
  {
    insts: "../TOT-chart-videos/In the Name of God.avi",
    venue: "../TOT-chart-videos/In the Name of God VENUE.mp4",
    instsStart: 6.157,
    venueStart: 5.070,
    songLength: 14 * 60 + 21.586,
    audio: "inthenameofgod.wav",
    card: "7 In the Name of God.png",
    mask: "mask.png",
    pro: "../TOT-chart-videos/In the Name of God PRO.avi",
    ch: "../TOT-chart-videos/In the Name of God CH.flv",
    proStart: 7.023,
    chStart: 5.550,
  },
]

[].each do |n|

  insts = songs[n][:insts]
  venue = songs[n][:venue]

  instsStart = songs[n][:instsStart]
  venueStart = songs[n][:venueStart]

  songLength = songs[n][:songLength]

  audio = songs[n][:audio]
  card = songs[n][:card]
  mask = songs[n][:mask]

  cmd = %W{
    ffmpeg
    -ss #{instsStart - cardFadeIn - cardHold} -i #{insts}
    -ss #{venueStart - cardFadeIn - cardHold} -i #{venue}
    -loop 1 -i #{card}
    -loop 1 -i #{mask}
    -itsoffset #{cardFadeIn + cardHold} -i #{audio}
    -filter_complex
  }

  cmd << %{
    [0:v] fps=60, scale=1920x1076 [insts];
    [1:v] fps=60, scale=1920x1080 [venue];
    [2] format=rgba, scale=1920x1080,
      fade=t=out:st=#{cardFadeIn + cardHold}:d=#{cardFadeOut}:alpha=1,
      fade=t=in:st=0:d=#{cardFadeIn}:color=black [card];
    [insts][3:v] alphamerge [masked];
    [venue][masked] overlay=y=4 [venueinsts];
    [venueinsts][card] overlay
  }

  cmd += %W{ -c:v libx264 -r 60 -map 4:a -t #{cardFadeIn + cardHold + songLength} #{n + 1}_RB3.mkv }

  system(*cmd)

end

[0, 1, 2, 3, 5, 6].each do |n|

  pro = songs[n][:pro]
  ch = songs[n][:ch]

  proStart = songs[n][:proStart]
  chStart = songs[n][:chStart]

  songLength = songs[n][:songLength]

  audio = songs[n][:audio]
  card = songs[n][:card]

  cmd = %W{
    ffmpeg
    -ss #{proStart - cardFadeIn - cardHold} -i #{pro}
    -ss #{chStart - cardFadeIn - cardHold} -i #{ch}
    -loop 1 -i #{card}
    -itsoffset #{cardFadeIn + cardHold} -i #{audio}
    -filter_complex
  }

  cmd << %{
    [0:v] fps=60, scale=1920x1076 [pro];
    [1:v] fps=60, scale=1920x1080, crop=x=0:y=540:w=1920:h=540 [ch];
    [2] format=rgba, scale=1920x1080,
      fade=t=out:st=#{cardFadeIn + cardHold}:d=#{cardFadeOut}:alpha=1,
      fade=t=in:st=0:d=#{cardFadeIn}:color=black [card];
    color=size=1920x1080:color=black:rate=60 [black];
    [black][pro] overlay=shortest=1:y=4 [pro2];
    [pro2][ch] overlay=shortest=1 [vids];
    [vids][card] overlay
  }

  cmd += %W{ -c:v libx264 -r 60 -map 3:a -t #{cardFadeIn + cardHold + songLength} #{n + 1}_CHPRO.mkv }

  system(*cmd)

end
