#!/usr/bin/env ruby

`echo src/Data/DTA/Serialize/*.erb`.split(/\s+/).each do |erb|
  hs = erb.clone
  hs['.erb'] = '.hs'
  `erb "#{erb}" > "#{hs}"`
end
