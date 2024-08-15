# Copyright 2007-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2 or later

PN = ebuild-mode
PV = $(shell sed '/^;.*[Vv]ersion/!d;s/[^0-9.]*\([^ \t]*\).*/\1/;q' \
	ebuild-mode.el)
P = $(PN)-$(PV)

TESTS = test/ebuild-mode-tests.el test/glep-mode-tests.el
DISTFILES = ebuild-mode.el ebuild-mode-keywords.el devbook-mode.el \
	gentoo-newsitem-mode.el glep-mode.el ebuild-mode.texi \
	ChangeLog Makefile keyword-generation.sh \
	$(TESTS)

ELCS = ebuild-mode.elc devbook-mode.elc gentoo-newsitem-mode.elc glep-mode.elc
INFOFILES = ebuild-mode.info

EMACS = emacs
EMACSFLAGS = -batch -q --no-site-file
BYTECOMPFLAGS = -eval "(add-to-list 'load-path nil)"

.PHONY: all keywords check dist clean

all: $(ELCS) $(INFOFILES)

%.elc: %.el
	$(EMACS) $(EMACSFLAGS) $(BYTECOMPFLAGS) \
		-f batch-byte-compile $<

%.info: %.texi
	makeinfo $<

keywords:
	./keyword-generation.sh

check:
	$(EMACS) $(EMACSFLAGS) $(BYTECOMPFLAGS) $(patsubst %,-l %,$(TESTS)) \
		-f ert-run-tests-batch-and-exit

dist: $(DISTFILES)
	tar -cJf $(P).tar.xz --transform='s%^%$(P)/%' $^
	tar -tJvf $(P).tar.xz

clean:
	-rm -f *~ *.tmp *.gz *.bz2 *.xz *.elc *.info
