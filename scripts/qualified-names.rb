#!/usr/bin/env ruby

qualified_imports = Dir.glob('packages/**/*.{hs,chs,x,y}').map { |f|
  File.read(f).scan(/import\s+qualified\s+([A-Za-z0-9\.]+)\s+as\s+([A-Za-z0-9]+)/)
}.flatten(1).uniq.sort_by { |x, y| x }
qualified_imports.each do |x, y|
  puts "import qualified #{x} as #{y}"
end
