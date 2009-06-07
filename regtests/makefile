############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2009, AdaCore                     #
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

TESTS = testme3 testme4 testme12 testme13 testme14 testme19 testme20 testme21\
	testme22 testme23 testme28 testme29 testme33 testme34 testme35 \
        testme36 testme37 testme39 testme40 testme41 testme42 blocks1 \
	blocks2 blocks5 blocks7 testme43 testme44 testme45 testme46 \
	testme47 testme48 testme49 testme50 testme51 testme52 testme55 \
	testme56 testme57 testme58 testme60 testme61 testme62 testme63 \
	testme65 testme66 testme67 testme68 testme69 testme70 testme71 \
	testme72 testme73 testme74 testme75 testme76 testme77

TESTS_TREE = testme testme2 testme5 testme6 testme7 testme8 testme11 testme15\
	testme16 testme17 testme18 testme24 testme25 testme26 testme27 \
	testme30 testme31 testme32 testme38 blocks3 blocks4 blocks6 testme53 \
	testme54 testme59 testme64

TESTS_CACHE = testme76

STANDALONE_TESTS := test_templates_if testcat testme9_run testme10_run

ifneq ($(T2A),)
STANDALONE_TESTS := $(STANDALONE_TESTS) tt2a
endif

ifeq ($(TP_XMLADA), Installed)
XTESTS			= translations_demo
STANDALONE_TESTS	:= $(STANDALONE_TESTS) check_mem
else
XTESTS	=
endif

TESTS_RUN      	= ${TESTS:%=%_run}
TESTS_TREE_RUN	= ${TESTS_TREE:%=%_run}
TESTS_CACHE_RUN = ${TESTS_CACHE:%=%_crun}

STD_TESTS      = tag_demo tcache regtst1 include tess test_templates_if \
		ts_assoc not_initialized include2 regtst4 dynamic \
		user_filter nullset
EXP_TESTS      = regtst2 regtst3 $(XTESTS)

MAIN_TESTS     = testme print_tree filter

EXECP          = ../$(BDIR)/$(LIBRARY_TYPE)/rbin

# Add relocatable DLL into the PATH for Windows

ifeq (${OS}, Windows_NT)
export PATH := $(EXECP)/../lib:../$(BDIR)/$(LIBRARY_TYPE)/lib:${PATH}
endif

DIFF_ARGS = -c -w

ifeq (${OS}, Windows_NT)
EXE=.exe
else
EXE=
endif

ALL_EXEC = $(STD_TESTS:%=%$(EXE)) $(EXP_TESTS:%=%$(EXE)) \
		$(MAIN_TESTS:%=%$(EXE))

force:

$(STD_TESTS): force
	echo $@
	$(GNAT) make -q -p -XLIBRARY_TYPE=static -XPRJ_BUILD=$(PRJ_BUILD) \
		-Pregtests $@
	-$(EXECP)/$@ > $@.res 2>&1
	-diff $(DIFF_ARGS) $@.out $@.res >> alldiffs

$(EXP_TESTS): force
	echo $@
	$(GNAT) make -q -p -XLIBRARY_TYPE=static -XPRJ_BUILD=$(PRJ_BUILD) \
		-Pregtests $@ -bargs -E
	-$(EXECP)/$@ > $@.res 2>&1
	-diff $(DIFF_ARGS) $@.out $@.res >> alldiffs

tt2a: force
	echo $@
	$(T2A) -e tmpl -d . -t ../tools/templates.tads -o t2a.tmp
	-sed -e 's,\\,/,g' t2a.tmp > t2a.res
	-diff $(DIFF_ARGS) t2a.out t2a.res >> alldiffs

print_tree: force
	echo $@
	-$(EXECP)/print_tree ftp.tmplt > ftp.res 2>&1
	-diff $(DIFF_ARGS) ftp.out ftp.res >> alldiffs

testcat: force
	echo $@
	$(GNAT) make -q -p -XLIBRARY_TYPE=static -XPRJ_BUILD=$(PRJ_BUILD) \
		-Pregtests testcat
	-$(EXECP)/testcat
	-diff $(DIFF_ARGS) testcat.out testcat.res >> alldiffs

testme9_run:
	echo $@
	-$(EXECP)/testme testme9.tmplt | tail -n +6 > testme9.res 2>&1
	-$(EXECP)/print_tree testme9.tmplt > testme9_pt.res 2>&1
	-diff $(DIFF_ARGS) testme9.out testme9.res >> alldiffs
	-diff $(DIFF_ARGS) testme9_pt.out testme9_pt.res >> alldiffs

testme10_run:
	echo $@
	-$(EXECP)/testme testme10.tmplt kut > testme10.res 2>&1
	-$(EXECP)/print_tree testme10.tmplt > testme10_pt.res 2>&1
	-diff $(DIFF_ARGS) testme10.out testme10.res >> alldiffs
	-diff $(DIFF_ARGS) testme10_pt.out testme10_pt.res >> alldiffs

check_mem: force
	echo $@
	$(GNAT) make -q -p -Pregtests check_mem -largs -lgmem
	$(EXECP)/check_mem 2 > check_mem.res
	gnatmem 3 -i gmem.out $(EXECP)/check_mem$(EXEEXT) 2 1248 \
		> check_mem.run1
	grep "   Final" check_mem.run1 > run1
	$(EXECP)/check_mem 30 >> check_mem.res
	gnatmem 3 -i gmem.out $(EXECP)/check_mem$(EXEEXT) 30 1249 \
		> check_mem.run2
	grep "   Final" check_mem.run2 > run2
	-diff -c -w run1 run2 1>> alldiffs 2>> alldiffs

$(TESTS_RUN):
	echo $@
	-$(EXECP)/testme ${@:%_run=%}.tmplt > ${@:%_run=%}.res 2>&1
	-diff $(DIFF_ARGS) ${@:%_run=%}.out ${@:%_run=%}.res >> alldiffs

$(TESTS_CACHE_RUN):
	echo $@
	-$(EXECP)/testme ${@:%_crun=%}.tmplt cache > ${@:%_crun=%}.res 2>&1
	-diff $(DIFF_ARGS) ${@:%_crun=%}.out ${@:%_crun=%}.res >> alldiffs

$(TESTS_TREE_RUN):
	echo $@
	-$(EXECP)/testme ${@:%_run=%}.tmplt > ${@:%_run=%}.res 2>&1
	-$(EXECP)/print_tree ${@:%_run=%}.tmplt > ${@:%_run=%}_pt.res 2>&1
	-diff $(DIFF_ARGS) ${@:%_run=%}.out ${@:%_run=%}.res >> alldiffs
	-diff $(DIFF_ARGS) ${@:%_run=%}_pt.out ${@:%_run=%}_pt.res >> alldiffs

init:
	-rm -f alldiffs
	$(GNAT) make -q -p -XLIBRARY_TYPE=static -XPRJ_BUILD=$(PRJ_BUILD) \
		-Pregtests $(MAIN_TESTS)

test:  init $(EXECP)/testme $(EXECP)/print_tree $(STANDALONE_TESTS) \
	$(STD_TESTS) $(EXP_TESTS) \
	$(TESTS_RUN) $(TESTS_TREE_RUN) $(TESTS_CACHE_RUN)
ifeq ($(TP_XMLADA), Installed)
	-rm -f ts.f.xml tsr.f.xml
	$(EXECP)/filter < ts.xml | sort > ts.f.xml
	$(EXECP)/filter < tsr.xml | sort > tsr.f.xml
	-diff -c ts.f.xml tsr.f.xml >> alldiffs
endif
	echo "### Any line displayed after this line indicate a problem in Templates_Parser"
	cat alldiffs
	test ! -s alldiffs

test_result:
	test ! -s alldiffs

clean:
ifeq ($(AWS),)
	-$(GNAT) clean -XLIBRARY_TYPE=static -XPRJ_BUILD=$(PRJ_BUILD) -Pregtests
endif
	-rm -f *.res *.o *.ali b~*.ad[sb] *.exe *~ check_mem.run* run1 run2
	-rm -f $(ALL_EXEC) gmem.out alldiffs
	-rm -f ts.xml tsr.xml m.xml ts.f.xml tsr.f.xml
