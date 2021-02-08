# === General Configuration
DOC=doc

PDF_COMPILER=pdflatex
CFLAGS=-interaction=nonstopmode --output-directory=$(PWD)

DVI_READER=evince
PS_READER=zathura
PDF_READER=zathura

### Public targets

## PDF
pdf: $(DOC).tex
	$(PDF_COMPILER) $(CFLAGS) $(DOC)

pdf_bibtex: $(DOC).tex
	$(PDF_COMPILER) $(CFLAGS) $(DOC)
	bibtex $(DOC).aux
	$(PDF_COMPILER) $(CFLAGS) $(DOC)
	$(PDF_COMPILER) $(CFLAGS) $(DOC)
	bibtex $(DOC).aux
	$(PDF_COMPILER) $(CFLAGS) $(DOC)
	$(PDF_COMPILER) $(CFLAGS) $(DOC)

view-pdf: $(DOC).pdf
	$(PDF_READER) $(DOC).pdf & disown

# DVI
dvi: $(DOC).tex
	latex $(CFLAGS) $(DOC)

dvi_bibtex: $(DOC).tex
	latex $(CFLAGS) $(DOC)
	bibtex $(DOC).aux
	latex $(CFLAGS) $(DOC)
	latex $(CFLAGS) $(DOC)
	bibtex $(DOC).aux
	latex $(CFLAGS) $(DOC)
	latex $(CFLAGS) $(DOC)

view-dvi: $(DOC).dvi
	$(DVI_READER) $(DOC).dvi & disown

# PS
ps: $(DOC).tex
	pslatex $(CFLAGS) $(DOC)

ps_bibtex: $(DOC).tex
	pslatex $(CFLAGS) $(DOC)
	bibtex $(DOC).aux
	pslatex $(CFLAGS) $(DOC)
	pslatex $(CFLAGS) $(DOC)
	bibtex $(DOC).aux
	pslatex $(CFLAGS) $(DOC)
	pslatex $(CFLAGS) $(DOC)

view-ps: $(DOC).ps
	$(PS_READER) $(DOC).ps & disown

clean:
	@rm $(DOC).{aux,bak,swp,synctex\(busy\).gz,synctex.gz,out,idx,ind} 2>/dev/null || true
	@rm $(DOC).{ilg,log,lof,lot,lol,spl,toc,blg,bbl,bcf,run,xml,fls} 2>/dev/null || true
	@rm $(DOC).{nav,vrb,snm,xdv,fdb_latexmk,maf,4tc,xref,tmp} 2>/dev/null || true
	@rm $(DOC).{pyc,pyo,pyg,mtc,mtc0} 2>/dev/null || true

remove:
	@rm $(DOC).{pdf,ps,dvi} 2>/dev/null || true


help:
	@echo ""
	@echo -e "\e[1;33mTo compile, type:\e[0m"
	@echo "	make target"
	@echo ""
	@echo -e "\e[1;33mSupported targets:\e[0m"
	@echo "	dvi		Compile to dvi"
	@echo "	pdf		Compile to pdf"
	@echo "	ps		Compile to ps"
	@echo "	view-dvi	View dvi with okular"
	@echo "	view-pdf	View pdf with zathura"
	@echo "	view-ps		View ps with zathura"
	@echo "	clean		Clean up the auxiliary files"
	@echo ""

