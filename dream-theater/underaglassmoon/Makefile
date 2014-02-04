title = Under a Glass Moon
artist = Dream Theater
package = underaglassmoon

clean:
	rm -rf gen

# MIDI

gen/jammit/2p/notes.mid: notes.mid
	mkdir -p $(@D)
	cp $< $@
	../../scripts/fixresolution $@

# Audio parts

gen/jammit/%/song-untimed.wav:
	mkdir -p $(@D)
	jammittools -t "$(title)" -r "$(artist)" -y D -a $@

gen/jammit/%/drums-untimed.wav:
	mkdir -p $(@D)
	jammittools -t "$(title)" -r "$(artist)" -y d -a $@

gen/jammit/%.wav: gen/jammit/%-untimed.wav
	sox $< $@ pad 1.704

#gen/album/%/song.wav: audio-album.*
#	mkdir -p $(@D)
#	sox $< $@ pad ???

gen/album/%/drums.wav: gen/album/%/song.wav
	mkdir -p $(@D)
	sox $< $@ pad 1 trim 0 1 # silence with correct channel number and sample rate

# Audio merge

gen/%/audio.ogg: gen/%/drums.wav gen/%/song.wav
	mkdir -p $(@D)
	sox --combine merge $+ $@

%.mogg: %.ogg
	ogg2mogg $< $@

# Album art

gen/%/cover.png_xbox: ../../covers/images-and-words.*
	mkdir -p $(@D)
	rb3albumart $< $@

# Folder assembly

gen/%/package/songs/songs.dta: songs.dta
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
