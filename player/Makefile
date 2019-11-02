.PHONY: build minify

build:
	mkdir -p build
	cp -R www/* build/
	pulp build --to build/app.js
	cp build/app.js build/app.min.js

minify: build
	./opt.rb build/app.js
	closure-compiler --language_in=ECMASCRIPT5 build/app.js > build/app.min.js
