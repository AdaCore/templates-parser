
# $Id$

.SILENT:

TESTS = testme3 testme4 testme12 testme13 testme14 testme19 testme20 testme21\
	testme22 testme23 testme28 testme29 testme33 testme34 testme35 \
        testme36 testme37 testme39 testme40 testme41 testme42 blocks1 \
	blocks2 blocks5 blocks7 testme43 testme44 testme45 testme46 \
	testme47 testme48 testme49 testme50 testme51 testme52 testme55 \
	testme56 testme57 testme58 testme60 testme61 testme62 testme63

TESTS_TREE = testme testme2 testme5 testme6 testme7 testme8 testme11 testme15\
	testme16 testme17 testme18 testme24 testme25 testme26 testme27 \
	testme30 testme31 testme32 testme38 blocks3 blocks4 blocks6 testme53 \
	testme54 testme59

STANDALONE_TESTS := test_templates_if testcat testme9_run testme10_run

ifeq ($(TP_XMLADA), Installed)
XTESTS			= translations_demo
STANDALONE_TESTS	:= $(STANDALONE_TESTS) check_mem
else
XTESTS	=
endif

TESTS_RUN      = ${TESTS:%=%_run}
TESTS_TREE_RUN = ${TESTS_TREE:%=%_run}

STD_TESTS      = tag_demo tcache regtst1 include tess test_templates_if \
		ts_assoc not_initialized include2 context regtst4
EXP_TESTS      = regtst2 regtst3 $(XTESTS)

MAIN_TESTS     = testme print_tree filter

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
	$(GNATMAKE) -q -Pregtests $@
	-./$@ > $@.res 2>&1
	-diff $(DIFF_ARGS) $@.out $@.res >> alldiffs

$(EXP_TESTS): force
	echo $@
	$(GNATMAKE) -q -Pregtests $@ -bargs -E
	-./$@ > $@.res 2>&1
	-diff $(DIFF_ARGS) $@.out $@.res >> alldiffs

print_tree: force
	echo $@
	-./print_tree ftp.tmplt > ftp.res 2>&1
	-diff $(DIFF_ARGS) ftp.out ftp.res >> alldiffs

testcat: force
	echo $@
	$(GNATMAKE) -q -Pregtests testcat
	-./testcat
	-diff $(DIFF_ARGS) testcat.out testcat.res >> alldiffs

testme9_run:
	echo $@
	-./testme testme9.tmplt | tail +6 > testme9.res 2>&1
	-./print_tree testme9.tmplt > testme9_pt.res 2>&1
	-diff $(DIFF_ARGS) testme9.out testme9.res >> alldiffs
	-diff $(DIFF_ARGS) testme9_pt.out testme9_pt.res >> alldiffs

testme10_run:
	echo $@
	-./testme testme10.tmplt kut > testme10.res 2>&1
	-./print_tree testme10.tmplt > testme10_pt.res 2>&1
	-diff $(DIFF_ARGS) testme10.out testme10.res >> alldiffs
	-diff $(DIFF_ARGS) testme10_pt.out testme10_pt.res >> alldiffs

check_mem: force
	echo $@
	$(GNATMAKE) -q -Pregtests check_mem -largs -lgmem
	./check_mem 2 > check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) 2 1248 > check_mem.run1
	grep "   Final" check_mem.run1 > run1
	./check_mem 30 >> check_mem.res
	gnatmem 3 -i gmem.out ./check_mem$(EXEEXT) 30 1249 > check_mem.run2
	grep "   Final" check_mem.run2 > run2
	-diff -c -w run1 run2 1>> alldiffs 2>> alldiffs

$(TESTS_RUN):
	echo $@
	-./testme ${@:%_run=%}.tmplt > ${@:%_run=%}.res 2>&1
	-diff $(DIFF_ARGS) ${@:%_run=%}.out ${@:%_run=%}.res >> alldiffs

$(TESTS_TREE_RUN):
	echo $@
	-./testme ${@:%_run=%}.tmplt > ${@:%_run=%}.res 2>&1
	-./print_tree ${@:%_run=%}.tmplt > ${@:%_run=%}_pt.res 2>&1
	-diff $(DIFF_ARGS) ${@:%_run=%}.out ${@:%_run=%}.res >> alldiffs
	-diff $(DIFF_ARGS) ${@:%_run=%}_pt.out ${@:%_run=%}_pt.res >> alldiffs

init:
	-rm -f alldiffs
	$(GNATMAKE) -q -Pregtests $(MAIN_TESTS)

test:  init testme print_tree $(STANDALONE_TESTS) $(STD_TESTS) $(EXP_TESTS) \
	$(TESTS_RUN) $(TESTS_TREE_RUN)
ifeq ($(TP_XMLADA), Installed)
	-rm -f ts.f.xml tsr.f.xml
	filter < ts.xml | sort > ts.f.xml
	filter < tsr.xml | sort > tsr.f.xml
	-diff -c ts.f.xml tsr.f.xml >> alldiffs
endif
	echo "### Any line displayed after this line indicate a problem in Templates_Parser"
	cat alldiffs

clean:
	-rm -f *.res *.o *.ali b~*.ad[sb] *.exe *~ check_mem.run* run1 run2
	-rm -f $(ALL_EXEC) gmem.out alldiffs
	-rm -f ts.xml tsr.xml m.xml ts.f.xml tsr.f.xml
