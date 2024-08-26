# Copyright 2007-2024 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2 or later

PN = ebuild-mode
PV = $(shell sed '/^;.*[Vv]ersion/!d;s/[^0-9.]*\([^ \t]*\).*/\1/;q' \
	ebuild-mode.el)
P = $(PN)-$(PV)

EMACS = emacs
EMACSFLAGS = -batch -q --no-site-file
BYTECOMPFLAGS = -eval "(add-to-list 'load-path nil)"

DISTFILES = ebuild-mode.el ebuild-mode-keywords.el devbook-mode.el \
	gentoo-newsitem-mode.el glep-mode.el ebuild-mode.texi \
	ChangeLog Makefile keyword-generation.sh \
	test/ebuild-mode-tests.el test/devbook-mode-tests.el \
	test/gentoo-newsitem-mode-tests.el test/glep-mode-tests.el \
	test/xemacs-test-wrapper.el

ELCS = ebuild-mode.elc gentoo-newsitem-mode.elc
TESTS = test/ebuild-mode-tests.el test/gentoo-newsitem-mode-tests.el
INFOFILES = ebuild-mode.info
ifeq ($(findstring xemacs,$(EMACS)),)
  ELCS += devbook-mode.elc glep-mode.elc
  TESTS += test/devbook-mode-tests.el test/glep-mode-tests.el
endif

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
ifeq ($(findstring xemacs,$(EMACS)),)
	$(EMACS) $(EMACSFLAGS) $(BYTECOMPFLAGS) $(patsubst %,-l %,$(TESTS)) \
		-f ert-run-tests-batch-and-exit
else
	$(EMACS) $(EMACSFLAGS) $(BYTECOMPFLAGS) \
		-eval "(add-to-list 'load-path \"test\")" \
		-l xemacs-test-wrapper -f batch-test-emacs $(TESTS)
endif

dist: $(DISTFILES)
	tar -cJf $(P).tar.xz --transform='s%^%$(P)/%' $^
	tar -tJvf $(P).tar.xz

clean:
	-rm -f *~ *.tmp *.gz *.bz2 *.xz *.elc *.info
