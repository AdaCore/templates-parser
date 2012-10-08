############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2012, AdaCore                     #
#                                                                          #
#  This is free software;  you can redistribute it  and/or modify it       #
#  under terms of the  GNU General Public License as published  by the     #
#  Free Software  Foundation;  either version 3,  or (at your option) any  #
#  later version.  This software is distributed in the hope  that it will  #
#  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty #
#  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     #
#  General Public License for  more details.                               #
#                                                                          #
#  You should have  received  a copy of the GNU General  Public  License   #
#  distributed  with  this  software;   see  file COPYING3.  If not, go    #
#  to http://www.gnu.org/licenses for a complete copy of the license.      #
############################################################################

.SILENT:

VERSION	= 11.7w

DEBUG        = false
TP_TASKING   = Standard_Tasking
LIBRARY_TYPE = static
PROCESSORS   = 2
HOST	     = $(shell gcc -dumpmachine)
TARGET	     = $(shell gcc -dumpmachine)

TR             = $(shell if [ -f /usr/bin/tr ]; then echo /usr/bin/tr; \
			else echo tr; fi)
DR_BUILD       = $(shell echo $(PRJ_BUILD) | $(TR) "[[:upper:]]" "[[:lower:]]")
BDIR           = .build/$(TARGET)/$(DR_BUILD)

prefix	= $(dir $(shell which gnatls))..

ENABLE_STATIC = true
ENABLE_SHARED =$(shell $(GNAT) make -c -q -p -XTARGET=$(TARGET) \
			-Pconfig/setup/test_shared 2>/dev/null && echo "true")
TP_XMLADA    = $(shell $(GNAT) make -c -q -p -XTARGET=$(TARGET) \
			-Pconfig/setup/test_xmlada 2>/dev/null \
		&& echo "Installed")

-include makefile.setup

ifeq ($(HOST), $(TARGET))
IS_CROSS	= false
GPROPTS		=
TPREFIX=$(prefix)
else
IS_CROSS	= true
GPROPTS		= --target=$(TARGET)
TPREFIX=$(prefix)/$(TARGET)
endif

GNAT		= gnat
GPRBUILD	= gprbuild
GPRCLEAN	= gprclean

#  Install directories

I_BIN	= $(TPREFIX)/bin
I_INC	= $(TPREFIX)/include/templates_parser
I_LIB	= $(TPREFIX)/lib/templates_parser
I_DOC	= $(TPREFIX)/share/doc/templates_parser
I_GPR	= $(TPREFIX)/lib/gnat
I_TGP	= $(TPREFIX)/lib/gnat/templates_parser

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

ifeq ($(DEBUG), true)
PRJ_BUILD=Debug
else
PRJ_BUILD=Release
endif

ifeq ($(TP_XMLADA),)
TP_XMLADA=Disabled
endif

ALL_OPTIONS = INCLUDES="$(INCLUDES)" LIBS="$(LIBS)" PRJ_BUILD="$(PRJ_BUILD)" \
		TP_XMLADA="$(TP_XMLADA)" GNAT="$(GNAT)" \
		PRJ_BUILD="$(PRJ_BUILD)" LIBRARY_TYPE="$(LIBRARY_TYPE)" \
		BDIR="$(BDIR)" DEFAULT_LIBRARY_TYPE="$(DEFAULT_LIBRARY_TYPE)" \
		ENABLE_SHARED="$(ENABLE_SHARED)" AWS="$(AWS)" \
		ENABLE_STATIC="$(ENABLE_STATIC)" TARGET="$(TARGET)"

GPROPTS += -XPRJ_BUILD=$(PRJ_BUILD) -XTP_XMLADA=$(TP_XMLADA) \
		-XPROCESSORS=$(PROCESSORS) -XTARGET=$(TARGET) \
		-XVERSION=$(VERSION)

build: setup_config tp_xmlada.gpr
ifeq ($(ENABLE_STATIC), true)
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=static \
		-Ptemplates_parser
endif
ifeq ($(ENABLE_SHARED), true)
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=relocatable \
		-Ptemplates_parser
endif
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE="$(LIBRARY_TYPE)" \
		-Ptools/tools

tp_xmlada.gpr: setup

run_regtests test: build
	$(MAKE) -C regtests $(ALL_OPTIONS) test

doc:
	$(MAKE) -C docs $(ALL_OPTIONS) doc
	echo Templates_Parser Documentation built with success.

setup:
ifeq ($(TP_XMLADA), Installed)
	cp config/tp_xmlada_installed.gpr tp_xmlada.gpr
else
	cp config/tp_xmlada_dummy.gpr tp_xmlada.gpr
endif
ifeq ($(ENABLE_STATIC), true)
	$(MKDIR) -p $(BDIR)/static/obj
	$(MKDIR) -p $(BDIR)/static/lib
endif
ifeq ($(ENABLE_SHARED), true)
	$(MKDIR) -p $(BDIR)/relocatable/obj
	$(MKDIR) -p $(BDIR)/relocatable/lib
endif
	echo "prefix=$(prefix)" > makefile.setup
	echo "DEFAULT_LIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE)" >> makefile.setup
	echo "LIBRARY_TYPE=$(LIBRARY_TYPE)" >> makefile.setup
	echo "ENABLE_STATIC=$(ENABLE_STATIC)" >> makefile.setup
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	echo "DEBUG=$(DEBUG)" >> makefile.setup
	echo "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	echo "TP_XMLADA=$(TP_XMLADA)" >> makefile.setup
	echo "TARGET=$(TARGET)" >> makefile.setup

setup_config:
	echo 'project TP_Config is' > $(CONFGPR)
	echo '   for Source_Dirs use ();' >> $(CONFGPR)
	echo '   Default_Library_Type := "$(DEFAULT_LIBRARY_TYPE)";' \
		>> $(CONFGPR)
	echo '   Tasking := "$(TP_TASKING)";' >> $(CONFGPR)
	echo '   Target := "$(TARGET)";' >> $(CONFGPR)
	echo 'end TP_Config;' >> $(CONFGPR)

install_dirs:
	$(MKDIR) -p $(DESTDIR)$(I_BIN)
	$(MKDIR) -p $(DESTDIR)$(I_INC)
ifeq ($(ENABLE_STATIC), true)
	$(MKDIR) -p $(DESTDIR)$(I_LIB)/static
endif
ifeq ($(ENABLE_SHARED), true)
	$(MKDIR) -p $(DESTDIR)$(I_LIB)/relocatable
endif
	$(MKDIR) -p $(DESTDIR)$(I_GPR)
	$(MKDIR) -p $(DESTDIR)$(I_TGP)
	$(MKDIR) -p $(DESTDIR)$(I_DOC)

install: install_dirs
	$(CP) src/*.ad* $(DESTDIR)$(I_INC)
ifeq ($(ENABLE_STATIC), true)
	$(CP) $(BDIR)/static/lib/* $(DESTDIR)$(I_LIB)/static
endif
ifeq ($(ENABLE_SHARED), true)
	$(CP) $(BDIR)/relocatable/lib/* $(DESTDIR)$(I_LIB)/relocatable
endif
	$(CP) $(BDIR)/$(LIBRARY_TYPE)/bin/* $(DESTDIR)$(I_BIN)
	$(CP) config/templates_parser.gpr $(DESTDIR)$(I_GPR)
	$(CP) config/tp_shared.gpr $(DESTDIR)$(I_TGP)
	$(CP) tp_xmlada.gpr $(DESTDIR)$(I_TGP)
	$(CP) $(CONFGPR) $(DESTDIR)$(I_TGP)
ifeq ($(TP_XMLADA), Installed)
	$(CP) xsrc/*.ad* $(DESTDIR)$(I_INC)
endif
	$(RM) -f $(DESTDIR)$(I_LIB)/../libtemplates_parser$(SOEXT)
ifeq ($(ENABLE_SHARED), true)
ifeq ($(OS), Windows_NT)
	$(LN) $(I_LIB)/relocatable/libtemplates_parser$(SOEXT) $(DESTDIR)$(I_BIN)
endif
endif
	-$(CP) docs/templates_parser*html $(DESTDIR)$(I_DOC)
	-$(CP) docs/templates_parser*pdf $(DESTDIR)$(I_DOC)
	-$(CP) docs/templates_parser*info* $(DESTDIR)$(I_DOC)

clean:
ifeq ($(AWS),)
ifeq ($(ENABLE_STATIC), true)
	-$(GPRCLEAN) -XLIBRARY_TYPE=static $(GPROPTS) \
		-Ptemplates_parser
endif
	-$(GPRCLEAN) -XLIBRARY_TYPE="$(LIBRARY_TYPE)" $(GPROPTS) \
		-Ptools/tools
ifeq ($(ENABLE_SHARED), true)
	-$(GPRCLEAN) -XLIBRARY_TYPE=relocatable $(GPROPTS) \
		-Ptemplates_parser
endif
endif
	-$(MAKE) -C docs clean $(ALL_OPTIONS)
	-$(MAKE) -C regtests clean $(ALL_OPTIONS)
	$(RM) -fr .build makefile.setup
