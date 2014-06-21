gen/%p/fof/notes.mid: gen/%p/notes.mid
	mkdir -p $(@D)
	cp $< $@

gen/%p/fof/album.png: ../../covers/$(cover-name).*
	mkdir -p $(@D)
	convert $< $@

gen/%p/fof/drums.ogg: gen/%p/drums.wav
	mkdir -p $(@D)
	sox $< $@

gen/%p/fof/rhythm.ogg: gen/%p/bass.wav
	mkdir -p $(@D)
	sox $< $@

gen/%p/fof/song.ogg: gen/%p/song.wav
	mkdir -p $(@D)
	sox $< $@

gen/%p/fof/song.ini: ../../template/fof-$(config).ini gen/%p/fof/notes.mid
	mkdir -p $(@D)
	cat $< \
		| sed "s/<TITLE>/$(title)/g" \
		| sed "s/<ARTIST>/$(artist)/g" \
		| sed "s/<ALBUM>/$(album)/g" \
		| sed "s/<GENRE>/$(genre)/g" \
		| sed "s/<YEAR>/$(year)/g" \
		| sed "s/<LENGTH>/`../../scripts/song-length $(word 2,$+)`/g" \
		> $@

gen/%p/fof-all:
	make \
		gen/$*p/fof/notes.mid \
		gen/$*p/fof/album.png \
		gen/$*p/fof/drums.ogg \
		gen/$*p/fof/rhythm.ogg \
		gen/$*p/fof/song.ogg \
		gen/$*p/fof/song.ini