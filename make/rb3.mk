# required variables:
# cover-name, package, title, artist

# Audio

%.mogg: %.ogg
	ogg2mogg $< $@

# Album art

gen/cover.png_xbox: ../../covers/$(cover-name).*
	mkdir -p $(@D)
	rb3albumart $< $@

# Folder assembly

gen/%p/rb3/songs/songs.dta: gen/%p/songs.dta
	mkdir -p $(@D)
	cp $< $@

gen/%p/rb3/songs/$(package)/$(package).mid: gen/%p/notes.mid
	mkdir -p $(@D)
	cp $< $@

gen/%p/rb3/songs/$(package)/$(package).mogg: gen/%p/audio.mogg
	mkdir -p $(@D)
	cp $< $@

gen/%p/rb3/songs/$(package)/gen/$(package)_keep.png_xbox: gen/cover.png_xbox
	mkdir -p $(@D)
	cp $< $@

# Make the package!

gen/%p/rb3.con: \
		gen/%p/rb3/songs/songs.dta \
		gen/%p/rb3/songs/$(package)/$(package).mid \
		gen/%p/rb3/songs/$(package)/$(package).mogg \
		gen/%p/rb3/songs/$(package)/gen/$(package)_keep.png_xbox
	rb3pkg -p "$(artist): $(title)" -d "Version: $*p" -f gen/$*p/rb3 $@
