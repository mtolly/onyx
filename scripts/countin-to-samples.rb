#!/usr/bin/env ruby

require './common'
require 'fileutils'

ARGV.each do |yaml_path|
  yaml = load_yaml_tree(yaml_path)
  countins = []
  yaml['plans'].each do |k, plan|
    if plan['countin']
      countins << plan['countin']
    end
  end
  countins.uniq!
  if countins.length > 1
    puts "  #{yaml_path}"
    puts "More than one countin found!"
  elsif countins.length == 1
    puts "  #{yaml_path}"
    if countins[0].any? { |k, v| !(v.is_a?(String)) }
      puts "Countin has a sample with extra stuff applied, skipping."
    else
      mid = "#{File.dirname(yaml_path)}/notes.mid"
      bin = File.open(mid, 'rb') { |h| h.read() }
      if bin.include?('AUDIO countin')
        puts "Already contains sample track."
      else
        tmp = '/tmp/onyxcountin.midtxt'
        system('onyx', 'midi-text', mid, '--to', tmp, exception: true, out: '/dev/null', err: '/dev/null')
        txt = File.read(tmp)
        addition = "\n\"AUDIO countin\" {"
        countins[0].each do |time, sample|
          addition += "\n  #{time}: \"[#{sample}]\";"
        end
        addition += "\n}"
        File.write(tmp, txt + addition)
        tmpmid = '/tmp/onyxcountin.mid'
        system('onyx', 'midi-text', tmp, '--to', tmpmid, exception: true, out: '/dev/null', err: '/dev/null')
        FileUtils.mv(tmpmid, mid)
        puts "Added countin track!"
      end
    end
  end
end
