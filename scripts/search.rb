#!/usr/bin/env ruby

require 'midilib'

ARGV.each do |arg|
  seq = MIDI::Sequence.new
  File.open(arg, 'rb') do |f|
    # if f.read().include?('[onyx close')
    #   puts arg
    # end
    seq.read(f) do |track, num_tracks, i|
      next unless track
      name = ''
      track.each do |event|
        next unless event.delta_time == 0
        if MIDI::MetaEvent === event and event.meta_type == 3
          name = event.data.map(&:chr).join('')
          break
        end
      end
      if ['PART GUITAR', 'PART BASS', 'PART KEYS', 'PART RHYTHM', 'PART GUITAR COOP'].any? { |x| name.end_with?(x) || name.end_with?(x + ' EXT') }
        puts "#{arg} #{name.inspect}"
        # has_sysex = track.any? { |e| MIDI::SystemExclusive === e }
        # if has_sysex
        #   puts "#{arg} #{name.inspect} has sysex"
        # end
      end
    end
  end
end
