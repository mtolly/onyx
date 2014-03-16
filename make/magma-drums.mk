gen/%/magma/drums.wav: gen/%/drums.wav
	mkdir -p $(@D)
	cp $< $@

gen/%/magma/song-countin.wav: gen/%/song-countin.wav
	mkdir -p $(@D)
	cp $< $@

gen/%/magma/cover.bmp: ../../covers/$(cover-name).*
	mkdir -p $(@D)
	convert $< -resize 256x256\! $@

gen/%/magma/notes.mid: gen/%/notes.mid
	mkdir -p $(@D)
	cp $< $@
	../../scripts/magma-clean $@

gen/%/magma/$(package).rbproj: ../../dta/magma-drums.dta
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
