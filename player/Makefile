.PHONY: build minify

build:
	pulp build --to www/app.js
	cp www/app.js www/app.min.js

minify: build
	./opt.rb www/app.js
	closure-compiler --language_in=ECMASCRIPT5 www/app.js > www/app.min.js
