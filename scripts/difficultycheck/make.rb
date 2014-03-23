#!/usr/bin/env ruby

mkdir = "C:\\MinGW\\msys\\1.0\\bin\\mkdir"

(50..450).each do |i|
  i = "0#{i}" if i < 100
  `#{mkdir} -p src/songs`
  `sed "s/<NUM>/#{i}/g" songs.dta > src/songs/songs.dta`
  `rb3pkg -f src #{i}.con`
  `rm -rf src`
end
