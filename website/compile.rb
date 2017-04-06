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
    'video' => song['video'],
  })
end

def makeDifficulties(instruments, difficulties)
  def instrument_index(inst)
    case inst
    when 'guitar'     then '1'
    when 'pro-guitar' then '2'
    when 'bass'       then '3'
    when 'pro-bass'   then '4'
    when 'drums'      then '5'
    when 'keys'       then '6'
    when 'pro-keys'   then '7'
    when 'vocal'      then '8'
    else                   '9' + inst
    end
  end
  instruments.sort_by { |inst, val| instrument_index(inst) }.map do |inst, val|
    val = 0 if val == false
    val = 1 if val == true
    dots =
      case difficulties[inst]
      when 1 then 'BBBBB'
      when 2 then 'WBBBB'
      when 3 then 'WWBBB'
      when 4 then 'WWWBB'
      when 5 then 'WWWWB'
      when 6 then 'WWWWW'
      when 7 then 'DDDDD'
      else        ''
      end
    dots.gsub!('B', '<img alt="" class="onyx-instrument-difficulty-dot" src="img/black.png">')
    dots.gsub!('W', '<img alt="" class="onyx-instrument-difficulty-dot" src="img/white.png">')
    dots.gsub!('D', '<img alt="" class="onyx-instrument-difficulty-dot" src="img/devil.png">')
    diff_name = %w{
      Warmup Apprentice Solid Moderate Challenging Nightmare Impossible
    }[difficulties[inst] - 1]
    inst_name =
      case inst
      when 'drums'
        '(Pro) Drums'
      when 'vocal'
        "Vocals (#{ val })"
      else
        inst.split('-').map(&:capitalize).join(' ')
      end
    if val != 0
      instrument_image =
        if inst == 'vocal'
          "vocal-#{val}"
        elsif inst == 'drums'
          "pro-drums"
        else
          inst
        end
      %{
        <span class="onyx-instrument">
          <img alt="#{inst_name}" title="#{inst_name}" src="img/icons-alpha/#{instrument_image}.png" class="onyx-instrument-icon">
          <span class="onyx-instrument-difficulty" title="#{diff_name}">#{dots}</span>
        </span>
      }
    end
  end
end

def makeTargetName(target_name, target)
  if target['game'] == 'rb3'
    if target['2x-bass-pedal']
      'Rock Band 3 (2x Bass Pedal)'
    else
      'Rock Band 3'
    end
  elsif target['game'] == 'ps'
    'Phase Shift'
  else
    target_name
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
            'author' => song['project']['metadata']['author'],
            'comments' => (song['project']['metadata']['comments'] || []).map do |comment|
              Kramdown::Document.new(comment).to_html
            end,
            'track-number' => song['project']['metadata']['track-number'],
            'targets' => (song['project']['targets'] || {}).map do |target_name, target|
              {
                'name' => makeTargetName(target_name, target),
                'url' => (song['urls'] || {})[target_name],
              }
            end.select { |obj| not obj['url'].nil? },
            'difficulties' => makeDifficulties(song['project']['instruments'] || {}, song['project']['metadata']['difficulty'] || {}),
            'video' => song['video']
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
