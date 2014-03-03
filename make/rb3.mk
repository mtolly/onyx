# required variables:
# cover-name, package, title, artist

# Audio

%.mogg: %.ogg
	ogg2mogg $< $@

# Album art

gen/%/cover.png_xbox: ../../covers/$(cover-name).*
	mkdir -p $(@D)
	rb3albumart $< $@

# Folder assembly

gen/%/rb3/songs/songs.dta: gen/%/songs.dta
	mkdir -p $(@D)
	cp $< $@

gen/%/rb3/songs/$(package)/$(package).mid: gen/%/notes.mid
	mkdir -p $(@D)
	cp $< $@

gen/%/rb3/songs/$(package)/$(package).mogg: gen/%/audio.mogg
	mkdir -p $(@D)
	cp $< $@

gen/%/rb3/songs/$(package)/gen/$(package)_keep.png_xbox: gen/%/cover.png_xbox
	mkdir -p $(@D)
	cp $< $@

# Make the package!

gen/%/rb3.con: \
		gen/%/rb3/songs/songs.dta \
		gen/%/rb3/songs/$(package)/$(package).mid \
		gen/%/rb3/songs/$(package)/$(package).mogg \
		gen/%/rb3/songs/$(package)/gen/$(package)_keep.png_xbox
	rb3pkg -p "$(artist): $(title)" -d "Version: $*" -f gen/$*/rb3 $@
