# Useful to compile all BSD/MIT license texts from dependencies.

licenses = Dir['*/LICENSE'] + Dir['*/LICENSE.txt'] + Dir['*/LICENSE.md'] + Dir['*/license.md'] + Dir['*/COPYING'] + Dir['*/COPYRIGHT']
licenses.sort!

licenses.each do |license|
  text = File.read(license)
  next if text.include? 'GNU GENERAL PUBLIC LICENSE'
  next if text.include? 'GNU LESSER GENERAL PUBLIC LICENSE'
  program = File.dirname(license)
  /-(\d|\.)+\Z/.match(program) do |md|
    program = md.pre_match
  end

  source =
    if %w{libogg flac libvorbis vorbis-tools libsamplerate kakasi}.include?(program)
      'C/C++'
    else
      'Haskell'
    end
  
  puts "=== #{program} [#{source}] ==="
  puts ""
  puts text
  puts ""

end
