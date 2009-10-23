# Copyright 2007-2009 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2

PN = gentoo-syntax
PV = $(shell sed '/^;.*[Vv]ersion/!d;s/[^0-9.]*\([^ \t]*\).*/\1/;q' \
	gentoo-syntax.el)
P = $(PN)-$(PV)

DISTFILES = gentoo-syntax.el ebuild-mode-keywords.el eselect-mode-keywords.el \
	gentoo-syntax.texi ChangeLog


.PHONY: all dist clean

all:

gentoo-syntax.info: gentoo-syntax.texi
	makeinfo $<

dist: $(DISTFILES)
	tar -cjf $(P).tar.bz2 --transform='s%^%$(P)/%' $^
	tar -tjvf $(P).tar.bz2

clean:
	-rm -f *~ *.tmp *.gz *.bz2 *.info
