#!/usr/bin/env ruby

raise "Usage: #{$0} prog.js" if ARGV.length != 1
js = File.read(ARGV[0])
js.gsub!(/Data_Int\.toNumber(\([^\(\)]+\))/) { $1 }
js.gsub!('Failed pattern match at ', '')
js.gsub!('Failed pattern match: ', '')
js.gsub!(/ line (\d+), column (\d+) - line (\d+), column (\d+): /) { " #{$1}:#{$2}-#{$3}:#{$4}: " }
File.write(ARGV[0], js)
