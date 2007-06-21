PN = gentoo-syntax
PV = $(shell sed '/^[ \t]*\* .*[Vv]ersion/!d;s/[^0-9.]*\([0-9.]*\).*/\1/;q' \
	ChangeLog)
P = $(PN)-$(PV)

DISTFILE = gentoo-syntax.el ebuild-mode.el


.PHONY: all dist clean

all:

dist: $(DISTFILE)
	bzip2 -c $< >$(P).el.bz2

clean:
	-rm -f *~ *.tmp *.gz *.bz2
