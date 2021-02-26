# Variables
DOCS=$(patsubst %.tex,%.pdf,$(wildcard *.tex))
TAR=$(wildcard *.cls) $(wildcard *.pdf) $(patsubst %.pdf,%.tex,$(wildcard *.pdf)) Makefile
ARCHIVE=lezioni

COMP=pdflatex
COPTS=-shell-escape -synctex=1 -interaction=nonstopmode -file-line-error --output-directory=$(PWD)

bibliography=no


# Targets
all: $(DOCS)

$(DOCS): %.pdf: %.tex
	$(COMP) $(COPTS) $<
	$(COMP) $(COPTS) $<

%: %.tex
	$(COMP) $(COPTS) $<
ifeq ($(bibliography),yes)
	bibtex $@.aux
	$(COMP) $(COPTS) $<
endif
	$(COMP) $(COPTS) $<
ifeq ($(bibliography),yes)
	bibtex $@.aux
	$(COMP) $(COPTS) $<
	$(COMP) $(COPTS) $<
endif

tarball:
	tar cjvf $(ARCHIVE)_`date +%F`.tar.bz2 $(TAR)

clean:
	@rm *.{~,aux,bak,swp,synctex\(busy\).gz,synctex.gz,out,idx,ind} 2>/dev/null || true
	@rm *.{ilg,log,lof,lot,lol,spl,toc,blg,bbl,bcf,run,xml,fls} 2>/dev/null || true
	@rm *.{nav,vrb,snm,xdv,fdb_latexmk,maf,4tc,xref,tmp} 2>/dev/null || true
	@rm *.{pyc,pyo,pyg,mtc,mtc0} 2>/dev/null || true

distclean: clean
	rm -f *.dvi *.ps *.pdf

.PHONY: clean distclean all
