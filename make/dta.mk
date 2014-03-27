rank-drum ?= 1
rank-band ?= 1

gen/%p/songs.dta: ../../dta/$(config).dta gen/%p/notes.mid
	cat $< \
		| sed "s/<TITLE>/$(title)/g" \
		| sed "s/<ARTIST>/$(artist)/g" \
		| sed "s/<PACKAGE>/$(package)/g" \
		| sed "s/<LENGTH>/`../../scripts/song-length $(word 2,$+)`/g" \
		| sed "s/<PREVIEW_BOUNDS>/`../../scripts/preview-bounds $(word 2,$+)`/g" \
		| sed "s/<RANK_DRUM>/$(rank-drum)/g" \
		| sed "s/<RANK_BAND>/$(rank-band)/g" \
		| sed "s/<GENRE>/$(genre)/g" \
		| sed "s/<GENDER>/$(gender)/g" \
		| sed "s/<YEAR>/$(year)/g" \
		| sed "s/<SUBGENRE>/$(subgenre)/g" \
		| sed "s/<ALBUM>/$(album)/g" \
		| sed "s/<NUMBER>/$(number)/g" \
		> $@