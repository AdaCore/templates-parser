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
VERSION	= 11.1
GNAT	= gnat

PRJ_BUILD    = Debug
TP_TASKING   = Standard_Tasking
TP_XMLADA    = Disabled
LIBRARY_TYPE = static

TR             = $(shell if [ -f /usr/bin/tr ]; then echo /usr/bin/tr; \
			else echo tr; fi)
DR_BUILD       = $(shell echo $(PRJ_BUILD) | $(TR) "[[:upper:]]" "[[:lower:]]")
BDIR           = .build/$(DR_BUILD)

INSTALL = $(dir $(shell which gnatls))..

ENABLE_SHARED=$(shell $(GNAT) make -c -q -p \
		-Pconfig/test_shared/test_shared 2>/dev/null && echo "true")

-include makefile.setup

I_BIN	= $(INSTALL)/bin
I_INC	= $(INSTALL)/include/templates_parser
I_LIB	= $(INSTALL)/lib/templates_parser
I_GPR	= $(INSTALL)/lib/gnat
I_TGP	= $(INSTALL)/lib/gnat/templates_parser
I_DOC	= $(INSTALL)/share/doc/templates_parser

CP	= cp -p
MKDIR	= mkdir -p

ifeq (${OS}, Windows_NT)
EXEEXT	= .exe
SOEXT	= .dll
LN	= cp -p
else
ifeq ($(UNAME), Darwin)
SOEXT   = .dylib
else
ifeq ($(UNAME), HP-UX)
SOEXT	= .sl
else
SOEXT	= .so
endif
endif
EXEEXT	=
LN	= ln -s
endif

CONFGPR	= config/tp_config.gpr

ifeq ($(DEFAULT_LIBRARY_TYPE),)
DEFAULT_LIBRARY_TYPE=static
endif

ifeq ($(LIBRARY_TYPE),)
LIBRARY_TYPE=static
endif

ALL_OPTIONS = INCLUDES="$(INCLUDES)" LIBS="$(LIBS)" MODE="$(MODE)" \
		TP_XMLADA="$(TP_XMLADA)" GNAT="$(GNAT)" \
		PRJ_BUILD="$(PRJ_BUILD)" LIBRARY_TYPE="$(LIBRARY_TYPE)" \
		BDIR="$(BDIR)" DEFAULT_LIBRARY_TYPE="$(DEFAULT_LIBRARY_TYPE)" \
		ENABLE_SHARED="$(ENABLE_SHARED)"

build: setup_config tp_xmlada.gpr
	$(GNAT) make -p -XLIBRARY_TYPE=static -Ptemplates_parser
	$(GNAT) make -p -XLIBRARY_TYPE=static -Ptools/tools
ifeq ($(ENABLE_SHARED), true)
	$(GNAT) make -p -XLIBRARY_TYPE=relocatable -Ptemplates_parser
endif

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
	$(MKDIR) -p $(BDIR)/static/obj
	$(MKDIR) -p $(BDIR)/static/lib
ifeq ($(ENABLE_SHARED), true)
	$(MKDIR) -p $(BDIR)/relocatable/obj
	$(MKDIR) -p $(BDIR)/relocatable/lib
endif
	echo "INSTALL=$(INSTALL)" > makefile.setup
	echo "DEFAULT_LIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE)" >> makefile.setup
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup

setup_config:
	echo 'project TP_Config is' > $(CONFGPR)
	echo '   for Source_Dirs use ();' >> $(CONFGPR)
	echo '   Default_Library_Type := "$(DEFAULT_LIBRARY_TYPE)";' \
		>> $(CONFGPR)
	echo '   Tasking := "$(TP_TASKING)";' >> $(CONFGPR)
	echo 'end TP_Config;' >> $(CONFGPR)

install_dirs:
	$(MKDIR) -p $(I_BIN)
	$(MKDIR) -p $(I_INC)
	$(MKDIR) -p $(I_LIB)/static
ifeq ($(ENABLE_SHARED), true)
	$(MKDIR) -p $(I_LIB)/relocatable
endif
	$(MKDIR) -p $(I_GPR)
	$(MKDIR) -p $(I_TGP)
	$(MKDIR) -p $(I_DOC)

install: install_dirs
	$(CP) src/*.ad* $(I_INC)
	$(CP) $(BDIR)/static/lib/* $(I_LIB)/static
ifeq ($(ENABLE_SHARED), true)
	$(CP) $(BDIR)/relocatable/lib/* $(I_LIB)/relocatable
endif
	$(CP) $(BDIR)/static/bin/* $(I_BIN)
	$(CP) config/templates_parser.gpr $(I_GPR)
	$(CP) config/tp_shared.gpr $(I_TGP)
	$(CP) tp_xmlada.gpr $(I_TGP)
	$(CP) $(CONFGPR) $(I_TGP)
ifeq ($(TP_XMLADA), Installed)
	$(CP) xsrc/*.ad* $(I_INC)
endif
	$(RM) -f $(I_LIB)/../libtemplates_parser$(SOEXT)
ifeq ($(ENABLE_SHARED), true)
	$(LN) $(I_LIB)/relocatable/libtemplates_parser$(SOEXT) $(I_LIB)/../
endif
	-$(CP) docs/templates_parser*html $(I_DOC)
	-$(CP) docs/templates_parser*pdf $(I_DOC)
	-$(CP) docs/templates_parser*info* $(I_DOC)

clean:
	-$(GNAT) clean -XLIBRARY_TYPE=static -Ptemplates_parser
	-$(GNAT) clean -XLIBRARY_TYPE=static -Ptools/tools
ifeq ($(ENABLE_SHARED), true)
	-$(GNAT) clean -XLIBRARY_TYPE=relocatable -Ptemplates_parser
endif
	-$(MAKE) -C docs clean $(ALL_OPTIONS)
	-$(MAKE) -C regtests clean $(ALL_OPTIONS)
	$(RM) -fr .build makefile.setup

distrib:
	-rm templates_parser-?.?.tar*
	tar cf templates_parser-$(VERSION).tar src/templates_parser*ad[sb] \
		config tools docs xsrc/*.ad* tp_shared.gpr \
		makefile \
		templates_parser.gpr
	gzip -9 templates_parser-$(VERSION).tar
