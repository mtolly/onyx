#!/usr/bin/env ruby

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
prev_level = headers[0][0]
puts "<ul>"
headers.each do |level, id, label|
  (level - prev_level).times { puts "<ul>" }
  (prev_level - level).times { puts "</ul>" }
  puts "<li><a href=\"##{id}\">#{label}</a></li>"
  prev_level = level
end
(prev_level - first_level).times { puts "</ul>" }
puts "</ul>"
