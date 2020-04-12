.PHONY: all enter clean check lint

all: build/example.xhtml build/example.pdf build/example.fo.pdf

enter:
	nix-shell -p libxml2 libxslt fop rnv

clean:
	rm -rf build

check: build/example.xml
	rnv ../../src/schspec-iso/schspec-iso.rnc "$<"

lint: build/example.xml
	xmllint --pretty 1 "$<" > build/example.xml.tmp
	mv build/example.xml.tmp "$<"

build/%.xml: %.scm
	mkdir -p build/figs
	./"$<" > "$@.tmp"
	mv "$@.tmp" "$@"

build/%.xhtml: build/%.xml ../../src/schspec-iso/style/default/schspec-iso-xhtml.xsl
	xsltproc ../../src/schspec-iso/style/default/schspec-iso-xhtml.xsl "$<" > "$@.tmp"
	mv "$@.tmp" "$@"
	ln -sf ../../../src/schspec-iso/style/default/schspec-iso-xhtml.css build/schspec-iso-xhtml.css

build/%.fo.pdf: build/%.xml ../../src/schspec-iso/style/default/schspec-iso-fo.xsl ../../src/schspec-iso/fopconfig.xml
	fop -c ../../src/schspec-iso/fopconfig.xml "$<" "$@"

build/%.pdf: build/%.tex
	(cd build; context "$(patsubst build/%.tex,%.tex,$<)"; )

build/%.tex: build/%.xml
	echo "\xdef\ArgInput{$(patsubst build/%.xml,%.xml,$<)}" > "$@.tmp"
	echo "\xdef\FigPath{}" >> "$@.tmp"
	echo '\input{../../../src/schspec-iso/style/context/schspec-iso.tex}' >> "$@.tmp"
	mv "$@.tmp" "$@"
