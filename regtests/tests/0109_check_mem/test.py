from test_support import *


def print_file(content):
    for line in content:
        print line

#  Build driver

gprbuild('check_mem')

#  Run driver (2 loops)

run('check_mem', ['2'], output_file='check_mem.res1')
run('gnatmem', ['3', '-i', 'gmem.out', './check_mem', "2"],
    output_file='check_mem.run1')

#  Run driver (30 loops)

run('check_mem', ['30'], output_file='check_mem.res2')
run('gnatmem', ['3', '-i', 'gmem.out', './check_mem', "30"],
    output_file='check_mem.run2')

#  Now check that final water-mark for run1 and run2 is equal
r1 = open('check_mem.run1').readlines()
r2 = open('check_mem.run2').readlines()

fr1 = "1"
fr2 = "2"

for item in r1:
    if item[0:8] == "   Final":
        fr1 = item
for item in r2:
    if item[0:8] == "   Final":
        fr2 = item

if fr1 != fr2:
    print "NOK"
    print "run 1 ---------------"
    print_file(r1)
    print "run 2 ---------------"
    print_file(r2)
    sys.exit(1)
else:
    print "OK"
    sys.exit(0)
