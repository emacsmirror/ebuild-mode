# Copyright 2007-2018 Christian Faulhammer and Ulrich Müller
# Distributed under the terms of the GNU General Public License v2 or later

PN = ebuild-mode
PV = $(shell sed '/^;.*[Vv]ersion/!d;s/[^0-9.]*\([^ \t]*\).*/\1/;q' \
	ebuild-mode.el)
P = $(PN)-$(PV)

DISTFILES = ebuild-mode.el ebuild-mode-keywords.el gentoo-newsitem-mode.el \
	glep-mode.el ebuild-mode.texi keyword-generation.sh ChangeLog


.PHONY: all dist clean

all:

ebuild-mode.info: ebuild-mode.texi
	makeinfo $<

dist: $(DISTFILES)
	tar -cJf $(P).tar.xz --transform='s%^%$(P)/%' $^
	tar -tJvf $(P).tar.xz

clean:
	-rm -f *~ *.tmp *.gz *.bz2 *.xz *.info
