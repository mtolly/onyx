gen/%p/magma/drums.wav: gen/%p/drums.wav
	mkdir -p $(@D)
	cp $< $@

gen/%p/magma/song-countin.wav: gen/%p/song-countin.wav
	mkdir -p $(@D)
	cp $< $@

gen/%p/magma/cover.bmp: ../../covers/$(cover-name).*
	mkdir -p $(@D)
	convert $< -resize 256x256\! $@

gen/%p/magma/notes.mid: gen/%p/notes.mid
	mkdir -p $(@D)
	cp $< $@
	../../scripts/magma-clean $@

gen/%p/magma/$(package).rbproj: ../../dta/magma-drums.dta
	mkdir -p $(@D)
	cat $< \
		| sed "s/<TITLE>/$(title)/g" \
		| sed "s/<ARTIST>/$(artist)/g" \
		| sed "s/<PACKAGE>/$(package)/g" \
		| sed "s/<PREVIEW_START>/$(preview-start)/g" \
		| sed "s/<GENRE>/$(genre)/g" \
		| sed "s/<GENDER>/$(gender)/g" \
		| sed "s/<YEAR>/$(year)/g" \
		| sed "s/<SUBGENRE>/$(subgenre)/g" \
		| sed "s/<ALBUM>/$(album)/g" \
		| sed "s/<NUMBER>/$(number)/g" \
		> $@

gen/%p/magma.rba: \
		gen/%p/magma/$(package).rbproj \
		gen/%p/magma/notes.mid \
		gen/%p/magma/cover.bmp \
		gen/%p/magma/song-countin.wav \
		gen/%p/magma/drums.wav
	magmyx -c3 $< $@
