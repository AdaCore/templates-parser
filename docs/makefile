
.SILENT: ada.sed doc clean

MAKEINFO        = $(shell which makeinfo 2> /dev/null)
DVIPS           = $(shell which dvips 2> /dev/null)
TEXI2DVI        = $(shell which texi2dvi 2> /dev/null)
PDFTEX          = $(shell which pdftex 2> /dev/null)

TMPLT	  = demo.tmplt user.tmplt table.tmplt table_section.tmplt \
	    table_if.tmplt matrix.tmplt table_block.tmplt table_inline.tmplt \
	    if_inline.tmplt

TMPLTTEXI = $(TMPLT:=.texi)

ADBFILES  = demo.adb user1.adb user2.adb table.adb table_section.adb \
	    table_if.adb matrix.adb table_inline.adb if_inline.adb

EXEFILES  = $(ADBFILES:.adb=)
ADAFILES  = $(ADBFILES)
ALIFILES  = $(ADSFILES:.ads=.ali) $(ADBFILES:.adb=.ali)
RESFILES  = $(ADBFILES:=.res)
TEXIFILES = $(ADBFILES:=.texi) $(ADSFILES:=.texi) $(ADAFILES_NORUN:=.texi) \
		$(TMPLTTEXI)

GFLAGS	  = -I../src -I../include

DOCS	  = templates_parser.dvi templates_parser.info templates_parser.html \
		templates_parser.txt templates_parser.ps templates_parser.pdf

ADA_KW    = use package is in out exception function with type constant \
		private of return range procedure begin end array record \
		else if then pragma access new case when subtype

APIFILES    = ../src/templates_parser.ads.texi \
	      ../src/templates_parser-debug.ads.texi \
	      ../src/templates_parser-utils.ads.texi \
	      ../xsrc/templates_parser-xml.ads.texi

ALLDEPS = $(ADBFILES) $(ADSFILES) $(EXEFILES) $(TEXIFILES) $(TMPLTTEXI) \
	$(RESFILES) $(APIFILES)

doc: $(ALLDEPS) $(DOCS) clean-tmp

%.dvi: %.texi
ifneq (${TEXI2DVI},)
	echo Building $@
	-${TEXI2DVI} --expand --clean --quiet $<
else
	@echo "--------------------------------------------------------"
	@echo "texi2dvi not found, cannot build DVI or PS documentation"
	@echo "--------------------------------------------------------"
endif

%.pdf: %.texi
ifneq (${TEXI2DVI},)
ifneq (${PDFTEX},)
	echo Building $@
	${TEXI2DVI} -p --expand --clean --quiet $<
else
	@echo "-----------------------------------------------"
	@echo "pdftex not fond, cannot build PDF documentation"
	@echo "-----------------------------------------------"
endif
else
	@echo "--------------------------------------------------"
	@echo "texi2dvi not found, cannot build PDF documentation"
	@echo "--------------------------------------------------"
endif

%.ps: %.dvi
ifneq (${DVIPS},)
	echo Building $@
	-${DVIPS} -q $< -o $@
else
	@echo "------------------------------------------------------"
	@echo "dvips not found, cannot build POSTSCRIPT documentation"
	@echo "------------------------------------------------------"
endif

%.info: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build INFO documentation"
	@echo "---------------------------------------------------"
endif

%.html: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --html --no-split --css-include=tp.css --ifinfo $<
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build HTML documentation"
	@echo "---------------------------------------------------"
endif

%.txt: %.texi
ifneq (${MAKEINFO},)
	echo Building $@
	-${MAKEINFO} --plaintext --no-headers $< --output $@
else
	@echo "---------------------------------------------------"
	@echo "makeinfo not found, cannot build TEXT documentation"
	@echo "---------------------------------------------------"
endif

$(ADBFILES) $(ADSFILES): all_sources.ada
	tail +3 all_sources.ada > sources.ada
	gnatchop -w sources.ada
	rm sources.ada

$(EXEFILES): $(ADBFILES) $(ADSFILES)
	$(GNATMAKE) -q -Pdocs $(EXEFILES)

%.adb.res: %.adb
	echo "@TPEXP{"         	  > $<.res
	./`basename $@ .adb.res` >> $<.res
	echo "}"		 >> $<.res

%.adb.html: %.adb
	`basename $@ .adb.html` >> $<.html

%.ads.texi: %.ads  ada.sed gentexifile
	./gentexifile $< NOGROUP

%.adb.texi: %.adb  %.adb.res ada.sed gentexifile
	./gentexifile $<

%.tmplt.texi: %.tmplt ada.sed gentexifile
	./gentexifile $<

ada.sed: makefile
	echo "s/\([^@_]\)@\([^@_]\)/\1@@\2/" > ada.sed
	echo "s/-- \(.*\)$$/-- @i{\1}/" >> ada.sed
	echo "/--/!s/\([^-][^-][^\"]*\)\"\([^\"]*\)\"/\1\"@i{\2}\"/g" \
		>> ada.sed
	echo "s/@@/@@@@/g" >> ada.sed
	echo "s/@_/@@_/g" >> ada.sed
	echo "s/_@/_@@/g" >> ada.sed
	for kw in $(ADA_KW); do \
		echo "s/^\([^-]* \)$$kw/\1@b{$$kw}/g" >> ada.sed; \
		echo "s/^\([^-]* \)$$kw/\1@b{$$kw}/g" >> ada.sed; \
		echo "s/^\( *\)$$kw /\1@b{$$kw} /g" >> ada.sed; \
		echo "s/^$$kw$$/@b{$$kw}/g" >> ada.sed; \
	done

force:

clean-tmp:
	-gnatclean -q -r -Pdocs $(EXEFILES)
	-rm -f $(EXEFILES) $(ADAFILES) $(TEXIFILES) *.res *.o *.ali genout \
		*.aux *.cp* *.fn *.ky *.pg *.toc *.tp *.vr *.dvi *.log *.ps \
		*.exe templates_parser*.ads.texi ada.sed \
		templates_parser.tar.gz

clean: clean-tmp
	-rm -f templates_parser.html templates_parser.pdf \
		templates_parser.info* templates_parser.txt
