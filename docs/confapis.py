
import os, os.path

API=['src/templates_parser.ads',
     'src/templates_parser-debug.ads',
     'src/templates_parser-utils.ads',
     'xsrc/templates_parser-xml.ads']

if os.path.exists("build/apirefs") == False:
    os.makedirs ("build/apirefs")

for file in API:
    out=True
    content = open("../"+file).readlines()
    fout = open("build/apirefs/"+os.path.basename(file),'w')
    for line in content:
        if line[0:4] == "end ":
            out = True
        if out == True:
            fout.write(line)
        if line == "private\n":
            out = False
            fout.write("   -- implementation removed\n")
