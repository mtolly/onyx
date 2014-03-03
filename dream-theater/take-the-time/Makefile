title = Take the Time
artist = Dream Theater
package = takethetime
cover-name = images-and-words
album-pad = 0.680

clean:
	rm -rf gen

# MIDI

gen/jammit/1p/notes.mid: notes.mid
	mkdir -p $(@D)
	cp $< $@
	../../scripts/fixresolution $@

gen/album/1p/notes.mid: notes.mid tempo-album.mid
	mkdir -p $(@D)
	../../scripts/replacetempos $+ $@

gen/%/2p/notes.mid: gen/%/1p/notes.mid
	mkdir -p $(@D)
	../../scripts/2xbasspedal $< $@

# Audio parts

gen/jammit/%/song.wav:
	mkdir -p $(@D)
	jammittools -T "$(title)" -R "$(artist)" -y D -a $@

gen/jammit/%/drums.wav:
	mkdir -p $(@D)
	jammittools -T "$(title)" -R "$(artist)" -y d -a $@

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

# Metadata

gen/%/songs.dta: gen/%/notes.mid songs.dta
	mkdir -p $(@D)
	../../scripts/dtafill -p $(package) -m $+ $@

# Rock Band 3

include ../../make/rb3.mk
