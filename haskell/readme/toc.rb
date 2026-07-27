#!/usr/bin/env ruby

# by Claude

require 'nokogiri'

html = Nokogiri::parse(File::read(__dir__ + '/index.html'))

headers = []
html.css('.readme').children.each do |el|
  el.name.match(/^h(\d+)$/) do |md|
    if el.attributes.has_key?('id')
      headers << [md[1].to_i, el.attributes['id'].value, el.text]
    end
  end
end

first_level = headers[0][0]
level = first_level
li_open = false
lines = []

# depth 0 is the outermost <ul>; a <ul> at depth d sits inside an <li> at depth d-1
add_ul = lambda { |depth, text| lines << ('  ' * (depth * 2)) + text }
add_li = lambda { |depth, text| lines << ('  ' * (depth * 2 + 1)) + text }

close_li = lambda do |depth|
  next unless li_open
  # keep leaf items on one line, but a <li> containing a nested <ul> needs its own
  if lines.last.start_with?(('  ' * (depth * 2 + 1)) + '<li>')
    lines[-1] += '</li>'
  else
    add_li[depth, '</li>']
  end
  li_open = false
end

add_ul[0, '<ul>']
headers.each do |header_level, id, label|
  depth = level - first_level
  while level < header_level
    # a nested <ul> must be inside an <li>; invent one if a level was skipped
    add_li[depth, '<li>'] unless li_open
    level += 1
    depth += 1
    add_ul[depth, '<ul>']
    li_open = false
  end
  while level > header_level
    close_li[depth]
    add_ul[depth, '</ul>']
    level -= 1
    depth -= 1
    li_open = true # the <li> holding that nested <ul> is still open
  end
  close_li[depth]
  add_li[depth, "<li><a href=\"##{id}\">#{label}</a>"]
  li_open = true
end
while level > first_level
  close_li[level - first_level]
  add_ul[level - first_level, '</ul>']
  level -= 1
  li_open = true
end
close_li[0]
add_ul[0, '</ul>']

puts lines
