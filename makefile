
.SILENT:

MODE	= RELEASE

VERSION	= 10.0

ALL_OPTIONS = INCLUDES="$(INCLUDES)" LIBS="$(LIBS)" MODE="$(MODE)" \
		TP_XMLADA="$(TP_XMLADA)" GNATMAKE="gnatmake"

all:
	echo ""
	echo "Targets:"
	echo ""
	echo "build     : build package"
	echo "test      : run a regression test"
	echo "doc       : build documentation"
	echo "distrib   : build tarball distribution"
	echo ""

force:

build: force
	gnatmake -Ptemplates_parser

test: build
	make -C regtests $(ALL_OPTIONS) test

doc:
	make -C docs $(ALL_OPTIONS) doc

clean:
	gnatclean -Ptemplates_parser
	make -C docs clean
	make -C regtests clean

distrib:
	-rm templates_parser-?.?.tar*
	tar cf templates_parser-$(VERSION).tar src/templates_parser*ad[sb] \
		src/ChangeLog include/*.ad* templates_parser.gpr
	gzip -9 templates_parser-$(VERSION).tar
