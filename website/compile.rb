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

def difficultyDots(n)
  dots =
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
  # explicit height/width get overridden by css
  dots.gsub!('B', '<img alt="" height="13px" width="13px" class="onyx-mode-difficulty-dot" src="img/black.png">')
  dots.gsub!('W', '<img alt="" height="13px" width="13px" class="onyx-mode-difficulty-dot" src="img/white.png">')
  dots.gsub!('D', '<img alt="" height="13px" width="13px" class="onyx-mode-difficulty-dot" src="img/devil.png">')
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
    modes_output = []
    modes.sort_by(&mode_index).each do |mode, info|
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
        if ['The Nag'].include?(song['project']['metadata']['title'])
          # hiding since this hasn't been released
          next
        end
        mode_name = '6-Fret (GHL)'
        mode_image = 'ghl'
      when 'pro-guitar'
        if [
          'In the Presence of Enemies (Part 1)',
          'Forsaken',
          'The Dark Eternal Night',
          'Repentance',
          'Prophets of War',
          'The Ministry of Lost Souls',
          'In the Presence of Enemies (Part 2)',
          'In the Presence of Enemies',
          'Summer Goddess',
          'Scoop Out',
          'The Black Widow Blues/The White Widow',
          'Sabotage',
          'Spark',
          'A Crimson Rose and a Gin Tonic',
          'Got a Match?',
          'Temple (Zelda 2)',
        ].include?(song['project']['metadata']['title'])
          # hiding these pro guitar/bass charts since they haven't been released
          next
        end
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
      end
      diff = info['difficulty']
      # explicit height gets overridden by css
      modes_output << %{
        <span class="onyx-mode">
          <img alt="#{mode_name}" height="27px" title="#{mode_name}" src="img/icons-alpha/#{mode_image}.png" class="onyx-mode-icon">
          <span class="onyx-mode-difficulty" title="#{difficultyName(diff)}">#{difficultyDots(diff)}</span>
        </span>
      }
    end
    unless modes_output.empty?
      output << %{
        <span class="onyx-part">
          <span class="onyx-part-name">#{part}</span>
          #{modes_output.join('')}
        </span>
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
  when 'ps'  then pieces << 'Phase Shift + Clone Hero'
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
      art_site += album_songs[0]['project']['metadata']['album'].tr("^A-Za-z0-9", '').downcase
      art_site += '.jpg'
      system 'convert', png, art_site

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
            'difficulties' => makeDifficulties(song['project']['parts'] || {}, song),
            'video' => song['video']
          }
        end.sort_by { |song| song['track-number'] },
        'art' => art_site,
        'year' => album_songs[0]['project']['metadata']['year'],
      }
    end.sort_by { |album| album['year'] },
  }
end.sort_by { |artist| artist['artist'].downcase }
data = {'artists' => artists}

page = Mustache.render(File.read('template/page.mustache'), data)
File.write('index.html', page)
