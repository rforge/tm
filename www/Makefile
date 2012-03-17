all: intro system

intro: publications.bib
	bib2bib -oc intro -ob intro.bib -c '$$key = "Rnews:Feinerer:2008"' publications.bib
	bibtex2html -d -r -nodoc -citefile intro -linebreak -nofooter intro.bib

system: publications.bib
	bib2bib -oc system -ob system.bib -c '$$key = "Feinerer_etal_2008" or $$key = "Feinerer_2008"' publications.bib
	bibtex2html -d -r -nodoc -citefile system -linebreak -nofooter -nf talk talk system.bib

.PHONY: clean
clean:
	-rm -f intro intro.bib system system.bib

.PHONY: realclean
realclean:
	-rm -f intro intro.bib intro_bib.html intro.html system system.bib system_bib.html system.html
