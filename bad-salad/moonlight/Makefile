title = Moonlight
artist = Bad Salad
package = moonlight
cover-name = puzzled
pledgemusic-trim = 0.326
pledgemusic-multiply = 0.6
album-pad = 3.572

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

gen/pledgemusic/%/drums-untimed.wav: BadSalad-Puzzled-2-Moonlight-Stem-Drums.*
	mkdir -p $(@D)
	sox -v $(pledgemusic-multiply) $< $@

gen/pledgemusic/%/song-untimed.wav: \
		BadSalad-Puzzled-2-Moonlight-Stem-Bass.* \
		BadSalad-Puzzled-2-Moonlight-Stem-Guitars.* \
		BadSalad-Puzzled-2-Moonlight-Stem-Keyboards.* \
		BadSalad-Puzzled-2-Moonlight-Stem-Vocals.*
	mkdir -p $(@D)
	sox --combine mix $(addprefix -v $(pledgemusic-multiply) ,$+) $@

gen/pledgemusic/%.wav: gen/pledgemusic/%-untimed.wav
	sox $< $@ trim $(pledgemusic-trim)

gen/album/%/song.wav: audio-album.*
	mkdir -p $(@D)
	../../scripts/audioconvert $< $@ pad $(album-pad) rate 44100 channels 2

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

gen/%/songs.dta: gen/%/notes.mid songs.dta
	mkdir -p $(@D)
	../../scripts/dtafill -p $(package) -m $+ $@

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
