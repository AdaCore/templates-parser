
# $Id$

.SILENT: ada.sed doc clean

TMPLT	  = demo.tmplt user.tmplt table.tmplt table_section.tmplt \
	    table_if.tmplt matrix.tmplt table_block.tmplt
TMPLTTEXI = $(TMPLT:=.texi)
ADBFILES  = demo.adb user1.adb user2.adb table.adb table_section.adb \
	    table_if.adb matrix.adb
EXEFILES  = $(ADBFILES:.adb=.exe)
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

doc: $(ALLDEPS) $(DOCS)

%.dvi: %.texi
	-texi2dvi --clean --quiet $<

%.pdf: %.ps
	-ps2pdf $< $@

%.ps: %.dvi
	-dvips -q $< -o $@

%.info: %.texi
	-makeinfo $<

%.html: %.texi
	-makeinfo --html --no-split --ifinfo $<

%.txt: %.texi
	-makeinfo --no-headers $< > $@

$(ADBFILES) $(ADSFILES): all_sources.ada
	tail +3 all_sources.ada > sources.ada
	gnatchop -w sources.ada
	rm sources.ada

%.exe: %.adb all_sources.ada
	$(GNATMAKE) -q -Pdocs $<
	touch $@

%.adb.res: %.adb %.exe
	echo "@example"          > $<.res
	echo "@cartouche"       >> $<.res
	echo "@group"           >> $<.res
	`basename $@ .adb.res`  >> $<.res
	echo "@end group"       >> $<.res
	echo "@end cartouche"   >> $<.res
	echo "@end example"     >> $<.res

%.adb.html: %.adb %.exe
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

clean:
	-rm -f $(EXEFILES) $(ADAFILES) $(TEXIFILES) *.res *.o *.ali \
		*.aux *.cp* *.fn *.ky *.pg *.toc *.tp *.vr *.dvi *.log *.ps \
		templates_parser*.ads.texi ada.sed \
		templates_parser.html templates_parser.pdf \
		templates_parser.info* \
		templates_parser.txt templates_parser.tar.gz
