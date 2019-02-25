sources = 01-Roswell/roswell.pdf \
	  02-Lem/lem.pdf \
	  09-RaspberryPi/RaspberryPi.pdf
target = techbookfest6.pdf
all: $(target)

$(target): $(sources)
	pdftk $(sources) cat output $@

%.pdf: %.md
	node_modules/markdown-pdf/bin/markdown-pdf $<

install: node_modules/markdown-pdf/bin/markdown-pdf

node_modules/markdown-pdf/bin/markdown-pdf: package.json
	yarn add markdown-pdf

package.json:
	yarn init -y
clean:
	rm -f $(sources)
	rm -f $(target)

clean-all: clean
	rm -rf node_modules
	rm -rf package.json
