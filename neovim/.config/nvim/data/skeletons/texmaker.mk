# === Configuration ===
DOC=
bibliography=no

COMP=lualatex
COPTS=-shell-escape -synctex=1 -interaction=nonstopmode -file-line-error --output-directory=$(PWD)
READER=zathura

# === Targets ===

$(DOC).pdf: $(DOC).tex
	$(COMP) $(COPTS) $<
ifeq ($(bibliography),yes)
	bibtex $(DOC).aux
	$(COMP) $(COPTS) $<
endif
	$(COMP) $(COPTS) $<
ifeq ($(bibliography),yes)
	bibtex $(DOC).aux
	$(COMP) $(COPTS) $<
	$(COMP) $(COPTS) $<
endif

view: $(DOC).pdf
	$(READER) $(DOC).pdf & disown

clean:
	@rm *.{~,aux,bak,swp,synctex\(busy\).gz,synctex.gz,out,idx,ind} 2>/dev/null || true
	@rm *.{ilg,log,lof,lot,lol,spl,toc,blg,bbl,bcf,run,xml,fls} 2>/dev/null || true
	@rm *.{nav,vrb,snm,xdv,fdb_latexmk,maf,4tc,xref,tmp} 2>/dev/null || true
	@rm *.{pyc,pyo,pyg,mtc,mtc0} 2>/dev/null || true

distclean: clean
	rm -f *.dvi *.ps *.pdf
