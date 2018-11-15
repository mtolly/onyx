#!/usr/bin/env ruby

imgs = Dir['www/images/*.png'].map { |f| File.basename(f, '.png') }.sort.map do |img|
  {path: img, const: "image_#{img.gsub('-', '_')}"}
end

imgs.each_with_index do |img, i|
  puts "#{img[:const]} :: ImageID"
  puts "#{img[:const]} = { index: #{i}, name: #{img[:path].inspect} }"
end

puts ""

puts "allImageIDs :: Array ImageID"
puts "allImageIDs = [#{imgs.map { |img| img[:const] }.join(', ')}]"

puts ""

