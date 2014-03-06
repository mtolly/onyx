title = Hell's Kitchen
artist = Dream Theater
package = hellskitchen
cover-name = falling-into-infinity
this-pad = 5.051
next-take = 5.696
next-fadeout = 4.500

clean:
	rm -rf gen

# MIDI

include ../../make/midi-two-tempos.mk

# Audio parts

gen/jammit/%/song.wav:
	mkdir -p $(@D)
	jammittools -T "$(title)" -R "$(artist)" -y D -a $@

gen/jammit/%/drums.wav:
	mkdir -p $(@D)
	jammittools -T "$(title)" -R "$(artist)" -y d -a $@

gen/album/%/this.wav: audio-this.*
	mkdir -p $(@D)
	../../scripts/audioconvert $< $@ pad $(this-pad) rate 44100 channels 2

gen/album/%/next.wav: audio-next.*
	mkdir -p $(@D)
	../../scripts/audioconvert $< $@ \
		fade t 0 $(next-take) $(next-fadeout) \
		rate 44100 \
		channels 2

gen/album/%/song.wav: gen/album/%/this.wav gen/album/%/next.wav
	sox --combine concatenate $+ $@

gen/album/%/drums.wav: gen/album/%/song.wav
	sox $< $@ pad 1 trim 0 1

include ../../make/countin.mk

# Audio merge

gen/%/audio.ogg: gen/%/drums.wav gen/%/song-countin.wav
	sox --combine merge $+ $@

# Metadata

gen/%/songs.dta: gen/%/notes.mid songs.dta
	mkdir -p $(@D)
	../../scripts/dtafill -p $(package) -m $+ $@

# Rock Band 3

include ../../make/rb3.mk
