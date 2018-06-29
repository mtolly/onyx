#!/usr/bin/env ruby

imgs = Dir['www/images/*.png'].map { |f| File.basename(f, '.png') }.sort.map do |img|
  {path: img, const: "Image_#{img.gsub('-', '_')}"}
end
puts "data ImageID"
imgs.each_with_index do |img, i|
  c = (i == 0 ? '=' : '|')
  puts "  #{c} #{img[:const]}"
end

puts ""

puts "allImageIDs :: Array ImageID"
puts "allImageIDs = [#{imgs.map { |img| img[:const] }.join(', ')}]"

puts ""

puts "imagePath :: ImageID -> String"
imgs.each do |img|
  puts "imagePath #{img[:const]} = \"#{img[:path]}\""
end

puts ""

puts "imageNumber :: ImageID -> Int"
imgs.each_with_index do |img, i|
  puts "imageNumber #{img[:const]} = #{i}"
end

puts ""

