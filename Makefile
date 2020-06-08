############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2020, AdaCore                     #
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

VERSION	= 20.0

DEBUG        = false
TP_TASKING   = Standard_Tasking
PROCESSORS   = 0
HOST         := $(shell gcc -dumpmachine)
TARGET       := $(shell gcc -dumpmachine)
prefix       := $(dir $(shell which gnatls))..
DEFAULT_LIBRARY_TYPE	= static

GNAT         := gnat

ENABLE_STATIC = true
ENABLE_SHARED := $(shell $(GNAT) make -c -q -p -XTARGET=$(TARGET) \
			-Pconfig/setup/test_shared 2>/dev/null && echo "true")

ifeq ($(shell gnat ls -Pxmlada 2>&1 | grep 'project file .* not found'),)
  TP_XMLADA := Installed
else
  TP_XMLADA := Disabled
endif

-include makefile.setup

ifeq ($(HOST), $(TARGET))
GPROPTS		=
TPREFIX=$(DESTDIR)$(prefix)
else
GPROPTS		= --target=$(TARGET)
TPREFIX=$(DESTDIR)$(prefix)/$(TARGET)
endif

MODE		= $(if $(filter-out true,$(DEBUG)),release,debug)
SDIR		= $(TARGET)/$(MODE)

GPRBUILD	= gprbuild
GPRINSTALL	= gprinstall
GPRCLEAN	= gprclean

#  Compute the default library kind, and possibly the other that are to
#  be built.

ifeq ($(DEFAULT_LIBRARY_TYPE),static)
  ifneq ($(ENABLE_STATIC),true)
    $(error static not enabled, cannot be the default)
  endif
  ifeq ($(ENABLE_SHARED),true)
    OTHER_LIBRARY_TYPE := relocatable
  endif
else
  ifneq ($(ENABLE_SHARED),true)
    $(error shared not enabled, cannot be the default)
  endif
  ifeq ($(ENABLE_STATIC),true)
    OTHER_LIBRARY_TYPE := static
  endif
endif

ifeq ($(DEBUG), true)
  PRJ_BUILD := Debug
else
  PRJ_BUILD := Release
endif

ALL_OPTIONS := \
 DEBUG \
 DEFAULT_LIBRARY_TYPE \
 ENABLE_SHARED \
 ENABLE_STATIC \
 GNAT \
 GPRBUILD \
 GPRCLEAN \
 PRJ_BUILD \
 PROCESSORS \
 SDIR \
 TARGET \
 TP_XMLADA \
 VERSION \
 prefix

override GPROPTS += $(foreach v, \
 PRJ_BUILD TP_XMLADA PROCESSORS TARGET VERSION \
 ,"-X$(v)=$($(v))")

GPR_DEFAULT = -XLIBRARY_TYPE=$(DEFAULT_LIBRARY_TYPE) \
		-XXMLADA_BUILD=$(DEFAULT_LIBRARY_TYPE)
GPR_OTHER   = -XLIBRARY_TYPE=$(OTHER_LIBRARY_TYPE) \
		-XXMLADA_BUILD=$(OTHER_LIBRARY_TYPE)

#######################################################################
#  build

build: tp_xmlada.gpr makefile.setup
	$(GPRBUILD) -p $(GPROPTS) $(GPR_DEFAULT) \
		--subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) -Ptemplates_parser
ifneq ($(OTHER_LIBRARY_TYPE),)
	$(GPRBUILD) -p $(GPROPTS) $(GPR_OTHER) \
		--subdirs=$(SDIR)/$(OTHER_LIBRARY_TYPE) -Ptemplates_parser
endif
	$(GPRBUILD) -p $(GPROPTS) $(GPR_DEFAULT) \
		--subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) -Ptools/tools

run_regtests test: build makefile.setup
	$(MAKE) -C regtests test

DOC_FORMATS := html latexpdf
build-doc: tp_xmlada.gpr makefile.setup
	$(MAKE) -C docs $(DOC_FORMATS)
	echo Templates_Parser Documentation built with success.

#######################################################################
#  setup

tp_xmlada.gpr:
ifeq ($(TP_XMLADA), Installed)
	cp config/tp_xmlada_installed.gpr tp_xmlada.gpr
else
	cp config/tp_xmlada_dummy.gpr tp_xmlada.gpr
endif

force:

makefile.setup: setup

setup: tp_xmlada.gpr force
	printf " $(foreach v,$(ALL_OPTIONS),$(v) = $($(v))\n)" > makefile.setup

#######################################################################
#  install

install-clean:
ifneq (,$(wildcard $(TPREFIX)/share/gpr/manifests/templates_parser))
	-$(GPRINSTALL) $(GPROPTS) --uninstall \
		--prefix=$(TPREFIX) templates_parser
endif

GPRINST_OPTS=-p -f --prefix=$(TPREFIX) \
	--build-var=LIBRARY_TYPE --build-var=TEMPLATES_PARSER_BUILD

install: install-clean
	$(GPRINSTALL) $(GPROPTS) $(GPR_DEFAULT) $(GPRINST_OPTS) \
		--subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) \
		--build-name=$(DEFAULT_LIBRARY_TYPE) -Ptemplates_parser
ifneq ($(OTHER_LIBRARY_TYPE),)
	$(GPRINSTALL) $(GPROPTS) $(GPR_OTHER) $(GPRINST_OPTS) \
		--build-name=$(OTHER_LIBRARY_TYPE) \
		--subdirs=$(SDIR)/$(OTHER_LIBRARY_TYPE) -Ptemplates_parser
endif
	$(GPRINSTALL) $(GPROPTS) $(GPR_DEFAULT) $(GPRINST_OPTS) \
		--mode=usage --subdirs=$(SDIR)/$(DEFAULT_LIBRARY_TYPE) \
		--install-name=templates_parser -Ptools/tools

#######################################################################
#  clean

clean:
	-$(GPRCLEAN) $(GPR_DEFAULT) $(GPROPTS) -Ptemplates_parser
	-$(GPRCLEAN) $(GPR_DEFAULT) $(GPROPTS) -Ptools/tools
ifneq ($(OTHER_LIBRARY_TYPE),)
	-$(GPRCLEAN) $(GPR_OTHER) $(GPROPTS) -Ptemplates_parser
endif
	$(MAKE) -C docs clean
	$(MAKE) -C regtests clean
	rm -f auto.cgpr config/setup/auto.cgpr
	rm -fr .build makefile.setup
	rm -f config/setup/foo.ali config/setup/foo.o tp_xmlada.gpr
	rm -f config/setup/foo.ads.std*
