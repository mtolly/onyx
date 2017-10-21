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

def makeDifficulties(parts)
  instruments = {}
  difficulties = {}
  if parts['guitar']
    if parts['guitar']['grybo']
      instruments['guitar'] = true
      difficulties['guitar'] = parts['guitar']['grybo']['difficulty']
    end
    if parts['guitar']['pro-guitar']
      instruments['pro-guitar'] = true
      difficulties['pro-guitar'] = parts['guitar']['pro-guitar']['difficulty']
    end
  end
  if parts['bass']
    if parts['bass']['grybo']
      instruments['bass'] = true
      difficulties['bass'] = parts['bass']['grybo']['difficulty']
    end
    if parts['bass']['pro-guitar']
      instruments['pro-bass'] = true
      difficulties['pro-bass'] = parts['bass']['pro-guitar']['difficulty']
    end
  end
  if parts['keys']
    if parts['keys']['grybo']
      instruments['keys'] = true
      difficulties['keys'] = parts['keys']['grybo']['difficulty']
    end
    if parts['keys']['pro-keys']
      instruments['pro-keys'] = true
      difficulties['pro-keys'] = parts['keys']['pro-keys']['difficulty']
    end
  end
  if parts['drums']
    if parts['drums']['drums']
      instruments['drums'] = true
      difficulties['drums'] = parts['drums']['drums']['difficulty']
    end
  end
  if parts['vocal']
    if parts['vocal']['vocal']
      instruments['vocal'] = parts['vocal']['vocal']['count']
      difficulties['vocal'] = parts['vocal']['vocal']['difficulty']
    end
  end
  if parts['violin']
    # if parts['violin']['grybo']
    #   instruments['violin-grybo'] = true
    #   difficulties['violin-grybo'] = parts['violin']['grybo']['difficulty']
    # end
    if parts['violin']['vocal']
      instruments['violin-vocal'] = parts['violin']['vocal']['count']
      difficulties['violin-vocal'] = parts['violin']['vocal']['difficulty']
    end
  end
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
      when 'violin-grybo'
        'Violin (GRYBO)'
      when 'violin-vocal'
        'Violin (Vocals)'
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
  pieces = []
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
            'difficulties' => makeDifficulties(song['project']['parts'] || {}),
            'video' => song['video']
          }
        end.sort_by { |song| song['track-number'] },
        'art' => art_site,
        'year' => album_songs[0]['project']['metadata']['year'],
      }
    end.sort_by { |album| album['year'] },
  }
end.sort_by { |artist| artist['artist'] }
data = {'artists' => artists}

page = Mustache.render(File.read('template/page.mustache'), data)
File.write('index.html', page)
