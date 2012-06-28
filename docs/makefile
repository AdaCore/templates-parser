############################################################################
#                             Templates_Parser                             #
#                                                                          #
#                      Copyright (C) 2003-2012, AdaCore                    #
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

.SILENT: doc clean

MAKEINFO        = $(shell which makeinfo 2> /dev/null)
TEXI2DVI        = $(shell which texi2dvi 2> /dev/null)
PDFTEX          = $(shell which pdftex 2> /dev/null)

TMPLT	  = demo.tmplt user.tmplt table.tmplt table_section.tmplt \
	    table_if.tmplt matrix.tmplt table_block.tmplt table_inline.tmplt \
	    if_inline.tmplt macro.tmplt

TMPLTTEXI = $(TMPLT:=.texi)

ADBFILES  = demo.adb user1.adb user2.adb table.adb table_section.adb \
	    table_if.adb matrix.adb table_inline.adb if_inline.adb macro.adb

EXEFILES  = $(ADBFILES:.adb=)
ADAFILES  = $(ADBFILES)
ALIFILES  = $(ADSFILES:.ads=.ali) $(ADBFILES:.adb=.ali)
RESFILES  = $(ADBFILES:=.res)
TEXIFILES = $(ADBFILES:=.texi) $(ADSFILES:=.texi) $(ADAFILES_NORUN:=.texi) \
		$(TMPLTTEXI)

GFLAGS	  = -I../src -I../include

GNATMAKE_FLAGS ?= -q

DOCS	  = templates_parser.info templates_parser.html \
		templates_parser.txt templates_parser.pdf

APIFILES    = ../src/templates_parser.ads.texi \
	      ../src/templates_parser-debug.ads.texi \
	      ../src/templates_parser-utils.ads.texi \
	      ../xsrc/templates_parser-xml.ads.texi

ALLDEPS = $(ADBFILES) $(ADSFILES) $(EXEFILES) $(TEXIFILES) $(TMPLTTEXI) \
	$(RESFILES) $(APIFILES)

doc: $(ALLDEPS) $(DOCS)

%.pdf: %.texi $(ADBFILES)
ifneq (${TEXI2DVI},)
ifneq (${PDFTEX},)
	echo Building $@
	${TEXI2DVI} -p --expand --clean --quiet $<
else
	@echo "------------------------------------------------"
	@echo "pdftex not found, cannot build PDF documentation"
	@echo "------------------------------------------------"
endif
else
	@echo "--------------------------------------------------"
	@echo "texi2dvi not found, cannot build PDF documentation"
	@echo "--------------------------------------------------"
endif

%.info: %.texi $(ADBFILES)
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build INFO documentation"
	@echo "---------------------------------------------------"
endif

%.html: %.texi $(ADBFILES)
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --html --no-split --css-include=tp.css --ifinfo $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build HTML documentation"
	@echo "---------------------------------------------------"
endif

%.txt: %.texi $(ADBFILES)
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --plaintext --no-headers $< --output $@
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build TEXT documentation"
	@echo "---------------------------------------------------"
endif

$(ADBFILES) $(ADSFILES): all_sources.ada
	$(GNAT) chop -w all_sources.ada

../tp_xmlada.gpr:
	cp ../config/tp_xmlada_dummy.gpr ../tp_xmlada.gpr

$(EXEFILES): ../tp_xmlada.gpr $(ADBFILES) $(ADSFILES)
	$(GNAT) make -p -XLIBRARY_TYPE=$(LIBRARY_TYPE) -XPRJ_BUILD=$(PRJ_BUILD) \
		-XTP_XMLADA=Disabled -Pdocs $(EXEFILES) $(GNATMAKE_FLAGS)

%.adb.res: %.adb
	echo "@TPEXP{"         	  > $<.res
	./`basename $@ .adb.res` >> $<.res
	echo "}"		 >> $<.res

%.adb.html: %.adb
	`basename $@ .adb.html` >> $<.html

%.ads.texi: %.ads gentexifile
	./gentexifile $< NOGROUP

%.adb.texi: %.adb  %.adb.res gentexifile
	./gentexifile $<

%.tmplt.texi: %.tmplt gentexifile
	./gentexifile $<

force:

clean:
ifeq ($(AWS),)
	-$(GNAT) clean -q -XLIBRARY_TYPE=static -XPRJ_BUILD=$(PRJ_BUILD) \
		-Pdocs $(EXEFILES)
endif
	-rm -f $(EXEFILES) $(ADAFILES) $(TEXIFILES) *.res *.o *.ali genout \
		*.aux *.cp* *.fn *.ky *.pg *.toc *.tp *.vr *.dvi *.log \
		*.exe templates_parser*.ads.texi ada.sed \
		templates_parser.tar.gz
	-rm -f templates_parser.html templates_parser.pdf \
		templates_parser.info* templates_parser.txt
