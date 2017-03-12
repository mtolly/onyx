#!/usr/bin/env ruby

require '../scripts/common'
require 'mustache'
require 'kramdown'

songs = []
load_yaml_tree('songs.yml').each do |song|
  songs.push({
    'project' => load_yaml_tree("#{song['file-path']}/song.yml"),
    'urls' => song['urls'],
  })
end

class Hash
  def map_values(&blk)
    hsh = {}
    self.each_pair do |k, v|
      hsh[k] = blk[v]
    end
    hsh
  end

  def tag_key(parent, children)
    self.map do |k, v|
      {
        parent => k,
        children => v,
      }
    end
  end
end

artists = songs.group_by { |s| s['project']['metadata']['artist'] }.map_values do |artist_songs|
  artist_songs.group_by { |s| s['project']['metadata']['album'] }.map_values do |album_songs|
    album_songs.map do |song|
      {
        'title' => song['project']['metadata']['title'],
        'comments' => (song['project']['metadata']['comments'] || []).map do |comment|
          Kramdown::Document.new(comment).to_html
        end,
        'targets' => (song['project']['targets'] || {}).map do |target_name, target|
          {
            'name' => target_name,
            'url' => song['urls'][target_name],
          }
        end.select { |obj| not obj['url'].nil? },
      }
    end
  end.tag_key('album', 'songs')
end.tag_key('artist', 'albums')
data = {'artists' => artists}

page = Mustache.render(File.read('template/page.mustache'), data)
File.write('index.html', page)
