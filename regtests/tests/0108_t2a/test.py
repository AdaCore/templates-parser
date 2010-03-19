from test_support import *

run('templates2ada', ["-e", "tmpl", "-d", ".", "-t", "./localtempl.tads",
                      "-o", "t2a.tmp"] )
file = open("t2a.tmp")
lines = file.readlines()
file.close()

for line in lines:
    print line[:-1].replace("\\", "/")
