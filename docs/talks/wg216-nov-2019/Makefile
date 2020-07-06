.PHONY: all

all: talk.html talk.pdf

talk.pdf: talk.md Makefile
	pandoc --to beamer --output talk.pdf talk.md
# -s
talk.html: talk.md Makefile talk.css
	pandoc --to revealjs --standalone --output talk.html talk.md \
        --css=talk.css \
        -V revealjs-url=./reveal.js-3.8.0 \
        -V theme=moon

#        -V revealjs-url="https://revealjs.com" \
#        -V revealjs-url=./reveal.js \

