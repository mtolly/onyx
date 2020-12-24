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
    'c3' => song['c3'],
    'hide-parts' => song['hide-parts'],
  })
end

def dotsLetters(n)
  case n
  when 1 then 'BBBBB'
  when 2 then 'WBBBB'
  when 3 then 'WWBBB'
  when 4 then 'WWWBB'
  when 5 then 'WWWWB'
  when 6 then 'WWWWW'
  when 7 then 'DDDDD'
  else        ''
  end
end

def dotsHTML(n)
  dots = dotsLetters(n)
  # explicit height/width get overridden by css
  dots.gsub!('B', '<img alt="" height="13px" width="13px" class="onyx-mode-difficulty-dot" src="img/black.png">')
  dots.gsub!('W', '<img alt="" height="13px" width="13px" class="onyx-mode-difficulty-dot" src="img/white.png">')
  dots.gsub!('D', '<img alt="" height="13px" width="13px" class="onyx-mode-difficulty-dot" src="img/devil.png">')
  dots
end

def dotsEmoji(n)
  dots = dotsLetters(n)
  dots.gsub!('B', '⚫')
  dots.gsub!('W', '⚪')
  dots.gsub!('D', '😈')
  dots
end

def difficultyName(n)
  %w{
    Warmup Apprentice Solid Moderate Challenging Nightmare Impossible
  }[n - 1]
end

def makeDifficulties(parts, song)
  part_index = proc do |x, y|
    case x
    when 'guitar' then '0'
    when 'bass'   then '1'
    when 'drums'  then '2'
    when 'keys'   then '3'
    when 'vocal'  then '4'
    else               '5' + x
    end
  end
  mode_index = proc do |x, y|
    case x
    when 'grybo'      then '0'
    when 'ghl'        then '1'
    when 'pro-guitar' then '2'
    when 'pro-keys'   then '3'
    when 'drums'      then '4'
    when 'vocal'      then '5'
    else                   '6' + x
    end
  end
  output = []
  parts.sort_by(&part_index).each do |part, modes|
    next if %w{guitar-ch bass-ch}.include?(part) # hack in Nurture
    next if %w{combo misc-lyrics}.include?(part) # hack in The Holiday Spirit Carries On
    modes_output = []
    modes.sort_by(&mode_index).each do |mode, info|
      if song['hide-parts'] and (song['hide-parts'].include?(part) or song['hide-parts'].include?("#{part}/#{mode}"))
        next
      end
      mode_name = '???'
      mode_image = 'no-image'
      case mode
      when 'grybo'
        mode_name = '5-Fret'
        mode_image =
          case part
          when 'bass' then 'bass'
          when 'keys' then 'keys'
          else             'guitar'
          end
      when 'ghl'
        mode_name = '6-Fret (GHL)'
        mode_image = 'ghl'
      when 'pro-guitar'
        if part == 'bass'
          mode_name = 'Pro Bass'
          mode_image = 'pro-bass'
        else
          mode_name = 'Pro Guitar'
          mode_image = 'pro-guitar'
        end
      when 'pro-keys'
        mode_name = 'Pro Keys'
        mode_image = 'pro-keys'
      when 'drums'
        mode_name = '(Pro) Drums'
        mode_image = 'pro-drums'
      when 'vocal'
        count = info['count']
        mode_name = "Vocals (#{count})"
        mode_image = "vocal-#{[count, 3].min}"
      else
        next
      end
      diff = info['difficulty']
      modes_output << {
        'name' => mode_name,
        'image' => mode_image,
        'difficulty' => difficultyName(diff),
        'dotsHTML' => dotsHTML(diff),
        'dotsEmoji' => dotsEmoji(diff),
      }
    end
    unless modes_output.empty?
      output << {
        'name' => part,
        'modes' => modes_output,
      }
    end
  end
  output
end

def makeTargetName(target_name, target)
  pieces = []
  if target['title']
    pieces << target['title']
    pieces << '-'
  end
  case target['game']
  when 'rb3' then pieces << 'Rock Band 3'
  when 'rb2' then pieces << 'Rock Band 2'
  when 'ps'  then pieces << 'Clone Hero + Phase Shift'
  end
  if target['label']
    pieces << target['label']
  elsif target['2x-bass-pedal']
    pieces << '(2x Bass Pedal)'
  end
  if pieces.empty?
    target_name
  else
    pieces.join(' ')
  end
end

artists = songs.group_by { |s| s['project']['metadata']['artist'] }.map do |artist_name, artist_songs|
  {
    'artist' => artist_name,
    'albums' => artist_songs.group_by { |s| s['project']['metadata']['album'] }.map do |album_name, album_songs|
      system "onyx shake #{album_songs[0]['dir']} gen/cover.png"
      png = "#{album_songs[0]['dir']}/gen/cover.png"
      art_site = 'album-art/'
      art_site += album_songs[0]['project']['metadata']['artist'].tr("^A-Za-z0-9", '').downcase
      if album_songs[0]['project']['metadata']['album']
        art_site += album_songs[0]['project']['metadata']['album'].tr("^A-Za-z0-9", '').downcase
      else
        art_site += 'noalbum'
      end
      art_site += '.jpg'
      system 'convert', png, art_site

      {
        'album' => album_name,
        'songs' => album_songs.map do |song|
          {
            'title' => song['project']['metadata']['title'],
            'author' => song['project']['metadata']['author'],
            'collab' => if song['project']['metadata']['author'] == 'Onyxite'
              then ''
              else ' [' + song['project']['metadata']['author'] + ']'
            end,
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
            'difficulties' => makeDifficulties(song['project']['parts'] || {}, song),
            'video' => song['video'],
            'c3' => song['c3']
          }
        end.sort_by { |song| song['track-number'] },
        'art' => art_site,
        'year' => album_songs[0]['project']['metadata']['year'],
      }
    end.sort_by { |album| album['year'] },
  }
end.sort_by { |artist| artist['artist'].downcase }
data = {'artists' => artists, 'website' => 'https://onyxite.org/customs/'}

page = Mustache.render(File.read('template/page.mustache'), data)
File.write('index.html', page)

page = Mustache.render(File.read('template/post.mustache'), data)
File.write('c3.bbcode', page)
