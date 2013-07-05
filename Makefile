# Copyright 2007-2013 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2 or later

PN = gentoo-syntax
PV = $(shell sed '/^;.*[Vv]ersion/!d;s/[^0-9.]*\([^ \t]*\).*/\1/;q' \
	gentoo-syntax.el)
P = $(PN)-$(PV)

DISTFILES = gentoo-syntax.el ebuild-mode-keywords.el gentoo-newsitem-mode.el \
	gentoo-syntax.texi keyword-generation.sh ChangeLog


.PHONY: all dist clean

all:

gentoo-syntax.info: gentoo-syntax.texi
	makeinfo $<

dist: $(DISTFILES)
	tar -cJf $(P).tar.xz --transform='s%^%$(P)/%' $^
	tar -tJvf $(P).tar.xz

clean:
	-rm -f *~ *.tmp *.gz *.bz2 *.xz *.info
