############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                         Copyright (C) 2003-2007                          #
#                                 AdaCore                                  #
#                                                                          #
#  This library is free software; you can redistribute it and/or modify    #
#  it under the terms of the GNU General Public License as published by    #
#  the Free Software Foundation; either version 2 of the License, or (at   #
#  your option) any later version.                                         #
#                                                                          #
#  This library is distributed in the hope that it will be useful, but     #
#  WITHOUT ANY WARRANTY; without even the implied warranty of              #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       #
#  General Public License for more details.                                #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with this library; if not, write to the Free Software Foundation, #
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          #
#                                                                          #
#  As a special exception, if other files instantiate generics from this   #
#  unit, or you link this unit with other files to produce an executable,  #
#  this  unit  does not  by itself cause  the resulting executable to be   #
#  covered by the GNU General Public License. This exception does not      #
#  however invalidate any other reasons why the executable file  might be  #
#  covered by the  GNU Public License.                                     #
############################################################################

.SILENT:

MODE	= RELEASE
VERSION	= 10.0
GNAT	= gnat

ALL_OPTIONS = INCLUDES="$(INCLUDES)" LIBS="$(LIBS)" MODE="$(MODE)" \
		TP_XMLADA="$(TP_XMLADA)" GNAT="$(GNAT)"

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
	$(GNAT) make -Ptemplates_parser

test: build
	make -C regtests $(ALL_OPTIONS) test

doc:
	make -C docs $(ALL_OPTIONS) doc
	echo Templates_Parser Documentation built with success.

clean:
	$(GNAT) clean -Ptemplates_parser
	make -C docs clean
	make -C regtests clean

distrib:
	-rm templates_parser-?.?.tar*
	tar cf templates_parser-$(VERSION).tar src/templates_parser*ad[sb] \
		src/ChangeLog include/*.ad* templates_parser.gpr
	gzip -9 templates_parser-$(VERSION).tar
