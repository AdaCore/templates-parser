############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2014, AdaCore                     #
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

VERSION	= 11.9.0w

DEBUG        = false
TP_TASKING   = Standard_Tasking
PROCESSORS   = 0
HOST	     = $(shell gcc -dumpmachine)
TARGET	     = $(shell gcc -dumpmachine)
prefix	     = $(dir $(shell which gnatls))..
DEFAULT_LIBRARY_TYPE	= static

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
TPREFIX=$(DESTDIR)$(prefix)
else
IS_CROSS	= true
GPROPTS		= --target=$(TARGET)
TPREFIX=$(DESTDIR)$(prefix)/$(TARGET)
endif

MODE		= $(if $(filter-out true,$(DEBUG)),release,debug)
SDIR		= $(TARGET)/$(MODE)

GNAT		= gnat
GPRBUILD	= gprbuild
GPRINSTALL	= gprinstall
GPRCLEAN	= gprclean

#  Install directories

ifeq (${OS}, Windows_NT)
EXEEXT	= .exe
else
EXEEXT	=
endif

#  Compute the default library kind, and possibly the other that are to
#  be built.

ifeq ($(DEFAULT_LIBRARY_TYPE),static)
ifneq ($(ENABLE_STATIC),true)
$(error static not enabled, cannot be the default)
else
ifeq ($(ENABLE_SHARED),true)
OTHER_LIBRARY_TYPE	= relocatable
endif
endif

else
ifneq ($(ENABLE_SHARED),true)
$(error shared not enabled, cannot be the default)
else
ifeq ($(ENABLE_STATIC),true)
OTHER_LIBRARY_TYPE	= static
endif
endif
endif

ifeq ($(DEBUG), true)
PRJ_BUILD=Debug
else
PRJ_BUILD=Release
endif

ifeq ($(TP_XMLADA),)
TP_XMLADA=Disabled
endif

ALL_OPTIONS = INCLUDES="$(INCLUDES)" PRJ_BUILD="$(PRJ_BUILD)" SDIR="$(SDIR)" \
		TP_XMLADA="$(TP_XMLADA)" GNAT="$(GNAT)" \
		PRJ_BUILD="$(PRJ_BUILD)" TARGET="$(TARGET)" \
		DEFAULT_LIBRARY_TYPE="$(DEFAULT_LIBRARY_TYPE)" \
		ENABLE_SHARED="$(ENABLE_SHARED)" \
		ENABLE_STATIC="$(ENABLE_STATIC)" \
		GPRBUILD="$(GPRBUILD)" GPRCLEAN="$(GPRCLEAN)"

GPROPTS += -XPRJ_BUILD=$(PRJ_BUILD) -XTP_XMLADA=$(TP_XMLADA) \
		-XPROCESSORS=$(PROCESSORS) -XTARGET=$(TARGET) \
		-XVERSION=$(VERSION)

#######################################################################
#  build

build: tp_xmlada.gpr
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) \
		--subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) -Ptemplates_parser
ifneq ($(OTHER_LIBRARY_TYPE),)
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=$(OTHER_LIBRARY_TYPE) \
		--subdirs=$(SDIR)/$(OTHER_LIBRARY_TYPE) -Ptemplates_parser
endif
	$(GPRBUILD) -p $(GPROPTS) -XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) \
		--subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) -Ptools/tools

run_regtests test: build
	$(MAKE) -C regtests $(ALL_OPTIONS) test

build-doc:
	$(MAKE) -C docs html latexpdf
	echo Templates_Parser Documentation built with success.

#######################################################################
#  setup

tp_xmlada.gpr: setup

setup:
ifeq ($(TP_XMLADA), Installed)
	cp config/tp_xmlada_installed.gpr tp_xmlada.gpr
else
	cp config/tp_xmlada_dummy.gpr tp_xmlada.gpr
endif
	echo "prefix=$(prefix)" > makefile.setup
	echo "DEFAULT_LIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE)" >> makefile.setup
	echo "ENABLE_STATIC=$(ENABLE_STATIC)" >> makefile.setup
	echo "ENABLE_SHARED=$(ENABLE_SHARED)" >> makefile.setup
	echo "DEBUG=$(DEBUG)" >> makefile.setup
	echo "PROCESSORS=$(PROCESSORS)" >> makefile.setup
	echo "TP_XMLADA=$(TP_XMLADA)" >> makefile.setup
	echo "TARGET=$(TARGET)" >> makefile.setup

#######################################################################
#  install

install-clean:
ifneq (,$(wildcard $(TPREFIX)/share/gpr/manifests/templates_parser))
	-$(GPRINSTALL) $(GPROPTS) --uninstall \
		--prefix=$(TPREFIX) templates_parser
endif

install: install-clean
	$(GPRINSTALL) $(GPROPTS) -p -f -XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) \
		--subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) \
		--prefix=$(TPREFIX) -Ptemplates_parser
ifneq ($(OTHER_LIBRARY_TYPE),)
	$(GPRINSTALL) $(GPROPTS) -p -f -XLIBRARY_TYPE=$(OTHER_LIBRARY_TYPE) \
		--prefix=$(TPREFIX) --build-name=$(OTHER_LIBRARY_TYPE) \
		--subdirs=$(SDIR)/$(OTHER_LIBRARY_TYPE) -Ptemplates_parser
endif
	$(GPRINSTALL) $(GPROPTS) -p -f -XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) \
		--prefix=$(TPREFIX) --mode=usage \
		--subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) \
		--install-name=templates_parser -Ptools/tools

#######################################################################
#  clean

clean:
	-$(GPRCLEAN) -XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) $(GPROPTS) \
		-Ptemplates_parser
	-$(GPRCLEAN) -XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) $(GPROPTS) \
		-Ptools/tools
ifneq ($(OTHER_LIBRARY_TYPE),)
	-$(GPRCLEAN) -XLIBRARY_TYPE=$(OTHER_LIBRARY_TYPE) $(GPROPTS) \
		-Ptemplates_parser
endif
	-$(MAKE) -C docs clean $(ALL_OPTIONS)
	-$(MAKE) -C regtests clean $(ALL_OPTIONS)
	rm -fr .build makefile.setup
