#!/usr/bin/env ruby

imgs = Dir['www/images/*.png'].map { |f| File.basename(f, '.png') }.sort.map do |img|
  "Image_#{img.gsub('-', '_')}"
end
puts "data ImageID"
imgs.each_with_index do |img, i|
  c = (i == 0 ? '=' : '|')
  puts "  #{c} #{img}"
end

puts ""

puts "allImageIDs :: Array ImageID"
puts "allImageIDs = [#{imgs.join ', '}]"

