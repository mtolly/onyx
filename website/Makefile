.PHONY: build upload upload-songs

build:
	./compile.rb
	sass css/styles.sass css/styles.css

upload:
	rsync -e "ssh -p 21098" -vrc * onyxhbdz@server122.web-hosting.com:~/public_html/ --exclude-from rsync-exclude --exclude download/

upload-songs:
	rsync -e "ssh -p 21098" -vrc download/ onyxhbdz@server122.web-hosting.com:~/public_html/download/ --exclude-from rsync-exclude
