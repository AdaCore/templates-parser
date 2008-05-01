############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                         Copyright (C) 2003-2008                          #
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
VERSION	= 11.0
GNAT	= gnat

PRJ_BUILD    = Debug
TP_TASKING   = Standard_Tasking
TP_XMLADA    = Disabled
LIBRARY_TYPE = relocatable

DR_BUILD       = $(shell echo $(PRJ_BUILD) | tr [[:upper:]] [[:lower:]])
BDIR           = .build/$(DR_BUILD)/$(LIBRARY_TYPE)

INSTALL = $(dir $(shell which gnatls))..
I_BIN	= $(INSTALL)/bin
I_INC	= $(INSTALL)/include/templates_parser
I_LIB	= $(INSTALL)/lib/templates_parser/$(LIBRARY_TYPE)
I_GPR	= $(INSTALL)/lib/gnat
I_TGP	= $(INSTALL)/lib/gnat/templates_parser
I_DOC	= $(INSTALL)/share/doc/templates_parser

CP	= cp -p
MKDIR	= mkdir -p

CONFGPR	= config/tp_config.gpr

ALL_OPTIONS = INCLUDES="$(INCLUDES)" LIBS="$(LIBS)" MODE="$(MODE)" \
		TP_XMLADA="$(TP_XMLADA)" GNAT="$(GNAT)" \
		PRJ_BUILD="$(PRJ_BUILD)" LIBRARY_TYPE="$(LIBRARY_TYPE)" \
		BDIR="$(BDIR)"

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

build: setup_config force tp_xmlada.gpr
	$(GNAT) make -p -Ptemplates_parser
	$(GNAT) make -p -Ptools/tools

test: build
	$(MAKE) -C regtests $(ALL_OPTIONS) test

doc:
	$(MAKE) -C docs $(ALL_OPTIONS) doc
	echo Templates_Parser Documentation built with success.

# If tp_xmlada.gpr does not exist, use the dummy one (no XML/Ada required)
tp_xmlada.gpr:
	cp config/tp_xmlada_dummy.gpr tp_xmlada.gpr

setup:
ifeq ($(TP_XMLADA), Installed)
	cp config/tp_xmlada_installed.gpr tp_xmlada.gpr
else
	cp config/tp_xmlada_dummy.gpr tp_xmlada.gpr
endif

setup_config:
	echo 'project TP_Config is' > $(CONFGPR)
	echo '   for Source_Dirs use ();' >> $(CONFGPR)
	echo '   Default_Library_Type := "$(LIBRARY_TYPE)";' >> $(CONFGPR)
	echo '   Tasking := "$(TP_TASKING)";' >> $(CONFGPR)
	echo 'end TP_Config;' >> $(CONFGPR)

install_dirs:
	$(MKDIR) -p $(I_BIN)
	$(MKDIR) -p $(I_INC)
	$(MKDIR) -p $(I_LIB)
	$(MKDIR) -p $(I_GPR)
	$(MKDIR) -p $(I_TGP)
	$(MKDIR) -p $(I_DOC)

install: install_dirs
	$(CP) src/*.ad* include/*.ad* $(I_INC)
	$(CP) $(BDIR)/lib/* $(I_LIB)
	$(CP) $(BDIR)/bin/* $(I_BIN)
	$(CP) config/templates_parser.gpr $(I_GPR)
	$(CP) config/tp_shared.gpr $(I_TGP)
	$(CP) tp_xmlada.gpr $(I_TGP)
	$(CP) $(CONFGPR) $(I_TGP)
ifeq ($(TP_XMLADA), Installed)
	$(CP) xsrc/*.ad* $(I_INC)
endif

clean:
	$(GNAT) clean -Ptemplates_parser
	$(GNAT) clean -Ptools
	$(MAKE) -C docs clean
	$(MAKE) -C regtests clean

distrib:
	-rm templates_parser-?.?.tar*
	tar cf templates_parser-$(VERSION).tar src/templates_parser*ad[sb] \
		include/*.ad* config tools docs xsrc/*.ad* shared.gpr \
		makefile \
		templates_parser.gpr
	gzip -9 templates_parser-$(VERSION).tar
