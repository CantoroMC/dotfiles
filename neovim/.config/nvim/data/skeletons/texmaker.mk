# === Configuration ===

DOC=texfile
TEXENGINE=pdflatex
TEXOPTIONS=-interaction=nonstopmode --output-directory=$(PWD)
BIBENGINE=bibtex
READER=zathura

# === Targets ===

compile: pdf

compileBib: pdfBib view

pdf:
	$(TEXENGINE) $(TEXOPTIONS) $(DOC)

pdfBib:
	$(TEXENGINE) $(TEXOPTIONS) $(DOC)
	$(BIBENGINE) $(DOC).aux
	$(TEXENGINE) $(TEXOPTIONS) $(DOC)
	$(TEXENGINE) $(TEXOPTIONS) $(DOC)
	$(BIBENGINE) $(DOC).aux
	$(TEXENGINE) $(TEXOPTIONS) $(DOC)
	$(TEXENGINE) $(TEXOPTIONS) $(DOC)

view: $(DOC).pdf
	$(READER) $(DOC).pdf & disown

remove:
	@rm $(DOC).pdf
