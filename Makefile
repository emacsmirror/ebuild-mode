# Copyright 2007-2008 Gentoo Foundation
# Distributed under the terms of the GNU General Public License v2

PN = gentoo-syntax
PV = $(shell sed '/^[ \t]*\* .*[Vv]ersion/!d;s/[^0-9.]*\([0-9.]*\).*/\1/;q' \
	ChangeLog)
P = $(PN)-$(PV)

DISTFILES = gentoo-syntax.el ebuild-mode-keywords.el \
	eselect-mode-keywords.el ChangeLog


.PHONY: all dist clean

all:

dist: $(DISTFILES)
	tar -cjf $(P).tar.bz2 --transform='s%^%$(P)/%' $^
	tar -tjvf $(P).tar.bz2

clean:
	-rm -f *~ *.tmp *.gz *.bz2
