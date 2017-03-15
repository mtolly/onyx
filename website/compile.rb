#!/usr/bin/env ruby

require '../scripts/common'
require 'mustache'
require 'kramdown'
require 'fileutils'

songs = []
load_yaml_tree('songs.yml').each do |song|
  songs.push({
    'dir' => song['file-path'],
    'project' => load_yaml_tree("#{song['file-path']}/song.yml"),
    'urls' => song['urls'],
  })
end

def makeDifficulties(instruments, difficulties)
  instruments.map do |instrument, val|
    val = 0 if val == false
    val = 1 if val == true
    if val != 0
      instrument_image =
        if instrument == 'vocal'
          "vocal-#{val}"
        elsif instrument == 'drums'
          "pro-drums"
        else
          instrument
        end
      %{<img src="img/icons-alpha/#{instrument_image}.png" class="onyx-instrument-icon">}
    end
  end
end

artists = songs.group_by { |s| s['project']['metadata']['artist'] }.map do |artist_name, artist_songs|
  {
    'artist' => artist_name,
    'albums' => artist_songs.group_by { |s| s['project']['metadata']['album'] }.map do |album_name, album_songs|
      system "onyx shake #{album_songs[0]['dir']} gen/cover.png"
      png = "#{album_songs[0]['dir']}/gen/cover.png"
      png_site = 'album-art/'
      png_site += album_songs[0]['project']['metadata']['artist'].tr("^A-Za-z0-9", '').downcase
      png_site += album_songs[0]['project']['metadata']['album'].tr("^A-Za-z0-9", '').downcase
      png_site += '.png'
      FileUtils.cp png, png_site

      {
        'album' => album_name,
        'songs' => album_songs.map do |song|
          {
            'title' => song['project']['metadata']['title'],
            'comments' => (song['project']['metadata']['comments'] || []).map do |comment|
              Kramdown::Document.new(comment).to_html
            end,
            'track-number' => song['project']['metadata']['track-number'],
            'targets' => (song['project']['targets'] || {}).map do |target_name, target|
              {
                'name' => target_name,
                'url' => song['urls'][target_name],
              }
            end.select { |obj| not obj['url'].nil? },
            'difficulties' => makeDifficulties(song['project']['instruments'] || {}, song['project']['metadata']['difficulty'] || {}),
          }
        end.sort_by { |song| song['track-number'] },
        'art' => png_site,
        'year' => album_songs[0]['project']['metadata']['year'],
      }
    end.sort_by { |album| album['year'] },
  }
end.sort_by { |artist| artist['artist'] }
data = {'artists' => artists}

page = Mustache.render(File.read('template/page.mustache'), data)
File.write('index.html', page)
