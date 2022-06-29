# workflows:
#
#  %.dia [dia]-> %.eps [ps2pdf]-> %.pdf
#  %.dot [dot]-> %.pdf [pdftops]-> %.eps
#  %.gnumeric [save as image from gnumeric]-> %.pdf [pdftops]-> %.eps

export TEXINPUTS := $(shell pwd)/../../common/pgf_2//:$(shell pwd)/../../common/pgfplots//::$TEXINPUTS

TEX_SRCS=\
	defs.tex \
	cps-type.tex \
	youyou.bib \
	llncs.cls \
#

# expecting .dia
DIA_SRCS=\
#

# expecting .dot
DOT_SRCS=\
#

# expecting .ps
GNUMERIC_SRCS=\
#

PDF_INPUT_FILES=$(DIA_SRCS:%.dia=%.pdf) $(DOT_SRCS:%.dot=%.pdf) $(GNUMERIC_SRCS:%.ps=%.pdf)
PDF_INPUT_PATHS=$(PDF_INPUT_FILES:%=build/%)
EPS_INPUT_PATHS=$(PDF_INPUT_PATHS:%.pdf=%.eps)

TEX_OPTIONS=-output-directory build

PDFLATEX=pdflatex $(TEX_OPTIONS)
BIBTEX=TEXMFOUTPUT=build bibtex
LATEX=latex $(TEX_OPTIONS)

.PHONY: default
default: all

export:
	export TEXINPUTS

.PHONY: all
all: build/cps-type.pdf
#all: $(BASE).tex $(BASE).pdf $(BASE).ps

$(BASE).tex: build/indexable.tex
	cp $< $@

$(BASE).pdf: build/cps-type.pdf
	cp $< $@

$(BASE).ps: build/cps-type.ps
	cp $< $@

build/cps-type.pdf: export $(TEX_SRCS) $(PDF_INPUT_PATHS)
	mkdir -p build
	@echo
	@echo
	@echo PDF_INPUT_PATHS=$(PDF_INPUT_PATHS)
	@echo
	@echo
	@echo
	@echo ----- $(PDFLATEX) 1... -----
	$(PDFLATEX) `basename $@ .pdf`.tex
	@echo ----- $(BIBTEX)... -----
	$(BIBTEX) build/`basename $@ .pdf`
	@echo ----- $(PDFLATEX) 2... -----
	$(PDFLATEX) `basename $@ .pdf`.tex
	@echo ----- $(PDFLATEX) 3... -----
	$(PDFLATEX) `basename $@ .pdf`.tex
	@echo ----- done -----

build/cps-type.dvi: export $(TEX_SRCS) $(EPS_INPUT_PATHS)
	@echo ----- $(PDFLATEX) 1... -----
	$(LATEX) `basename $@ .dvi`.tex
	@echo ----- $(BIBTEX)... -----
	$(BIBTEX) build/`basename $@ .dvi`
	@echo ----- $(PDFLATEX) 2... -----
	$(LATEX) `basename $@ .dvi`.tex
	@echo ----- $(PDFLATEX) 3... -----
	$(LATEX) `basename $@ .dvi`.tex
	@echo ----- done -----

build/cps-type.ps: build/cps-type.dvi
	dvips -o $@ $<

build/%.eps: from_dia/%.dia
	mkdir -p build
	dia --export=$@ $<

build/%.pdf: from_dot/%.dot
	mkdir -p build
	dot -Tpdf -o$@ $<

build/%.eps: from_gnumeric/%.ps
	mkdir -p build
	ps2epsi $< $@

build/%.eps: build/%.pdf
	pdftops -eps $< $@

build/%.pdf: build/%.eps
	ps2pdf -dEPSCrop $< $@

# ACM index needs the raw TEX and the *used* bibliography BBL
build/indexable.tex: cps-type.tex build/cps-type.pdf
	cat cps-type.tex build/cps-type.bbl > $@

.PHONY: clean
clean:
	rm -rf build *~

.PHONY: wordcount
wordcount:
	detex cps-type.tex | wc -w

grammar.txt: cps-type.tex
	sed -e 's/~\\ref{[^}]*}/ 42/g' -e 's/~\\cite{[^}]*}//g' cps-type.tex | detex | uniq | tr '[\n]' '[\\]' | sed 's/\([^\\]\)\\\([^\\]\)/\1 \2/g' | tr '[\\]' '[\n]' > grammar.txt
