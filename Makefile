CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=
VERSION					      := 0.0.1

all : haskell doc

haskell : src/Main.hs src/*.hs
	runhaskell Setup.lhs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.lhs build $(CABAL-BUILD-FLAGS)

doc: doc/report.ps
	cd doc && ps2pdf report.ps report.pdf

doc/report.ps: doc/report.dvi
	cd doc && dvips report.dvi

doc/report.dvi: doc/report.tex doc/references.bib
	cd doc && latex report.tex && bibtex report && latex report.tex && latex report.tex

doc/report.tex: doc/report.lhs
	cd doc && lhs2tex -o report.tex report.lhs

dist: all
	cabal sdist
	cp dist/strictness-$(VERSION).tar.gz dist/submit.tgz


testdist: dist
	cp dist/strictness-$(VERSION).tar.gz /tmp
	cd /tmp && tar xvzf strictness-$(VERSION).tar.gz && cd /tmp/strictness-$(VERSION) && cabal configure && cabal build