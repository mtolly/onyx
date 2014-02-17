title = Sacrificed Sons
artist = Dream Theater
package = sacrificedsons
cover-name = octavarium
jammit-trim = 46.526
album-trim = 45.914
fadein = 6.866

clean:
	rm -rf gen

# MIDI

gen/%/1p/notes.mid: notes.mid
	mkdir -p $(@D)
	cp $< $@
	../../scripts/fixresolution $@

gen/%/2p/notes.mid: gen/%/1p/notes.mid
	mkdir -p $(@D)
	../../scripts/2xbasspedal $< $@

# Audio parts

gen/jammit/%/song-untimed.wav:
	mkdir -p $(@D)
	jammittools -t "$(title)" -r "$(artist)" -y D -a $@

gen/jammit/%/drums-untimed.wav:
	mkdir -p $(@D)
	jammittools -t "$(title)" -r "$(artist)" -y d -a $@

gen/jammit/%.wav: gen/jammit/%-untimed.wav
	sox $< $@ trim $(jammit-trim) fade t $(fadein)

gen/album/%/song.wav: audio-album.*
	mkdir -p $(@D)
	sox $< $@ trim $(album-trim) fade t $(fadein) rate 44100 channels 2

gen/album/%/drums.wav: gen/album/%/song.wav
	sox $< $@ pad 1 trim 0 1

gen/%/countin.wav: gen/%/notes.mid ../../sound/hihat-foot.wav
	../../scripts/countin $+ $@

gen/%/song-countin.wav: gen/%/song.wav gen/%/countin.wav
	sox --combine mix -v 1 $+ $@

# Audio merge

gen/%/audio.ogg: gen/%/drums.wav gen/%/song-countin.wav
	sox --combine merge $+ $@

%.mogg: %.ogg
	ogg2mogg $< $@

# Album art

gen/%/cover.png_xbox: ../../covers/$(cover-name).*
	mkdir -p $(@D)
	rb3albumart $< $@

# Metadata

gen/%/songs.dta: notes.mid songs.dta
	mkdir -p $(@D)
	../../scripts/dtafill $(package) $+ $@

# Folder assembly

gen/%/package/songs/songs.dta: gen/%/songs.dta
	mkdir -p $(@D)
	cp $< $@

gen/%/package/songs/$(package)/$(package).mid: gen/%/notes.mid
	mkdir -p $(@D)
	cp $< $@

gen/%/package/songs/$(package)/$(package).mogg: gen/%/audio.mogg
	mkdir -p $(@D)
	cp $< $@

gen/%/package/songs/$(package)/gen/$(package)_keep.png_xbox: gen/%/cover.png_xbox
	mkdir -p $(@D)
	cp $< $@

# Make the package!

gen/%/package.con: \
		gen/%/package/songs/songs.dta \
		gen/%/package/songs/$(package)/$(package).mid \
		gen/%/package/songs/$(package)/$(package).mogg \
		gen/%/package/songs/$(package)/gen/$(package)_keep.png_xbox
	rb3pkg -p "$(artist): $(title)" -d "Version: $*" -f gen/$*/package $@
