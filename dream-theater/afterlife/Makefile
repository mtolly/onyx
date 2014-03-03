title = Afterlife
artist = Dream Theater
package = dtafterlife
# package name "afterlife" doesn't work
# probably conflicts with RB3 disc files relating to A7X afterlife
cover-name = when-dream-and-day-unite
this-pad = 2.704
next-take = 10.383
next-fadeout = 1.357

clean:
	rm -rf gen

# MIDI

gen/album/1p/notes.mid: notes.mid
	mkdir -p $(@D)
	cp $< $@
	../../scripts/fixresolution $@

gen/%/2p/notes.mid: gen/%/1p/notes.mid
	mkdir -p $(@D)
	../../scripts/2xbasspedal $< $@

# Audio parts

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
