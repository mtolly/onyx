.PHONY: build minify

build:
	pulp browserify -O --to app.js
	cp app.js app.min.js

minify:
	closure-compiler app.js > app.min.js
