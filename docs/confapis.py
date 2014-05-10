
import os
import os.path

API = ['src/templates_parser.ads',
       'src/templates_parser-debug.ads',
       'src/templates_parser-utils.ads',
       'xsrc/templates_parser-xml.ads']

if os.path.exists("build/apirefs") is False:
    os.makedirs("build/apirefs")

for path in API:
    try:
        fin = open("../" + path)
        fout = open("build/apirefs/" + os.path.basename(path), 'w')
        outside_private_part = True
        for line in fin:
            if line.startswith("end "):
                outside_private_part = True
            if outside_private_part:
                fout.write(line)
            if line == "private\n":
                outside_private_part = False
                fout.write("   -- implementation removed\n")
    finally:
        if fin:
            fin.close()
        if fout:
            fout.close()
