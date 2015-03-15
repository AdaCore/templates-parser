from test_support import *

Current = "EndTag"


def set_start(str):
    global Current
    if str.find("SimpleTag") != -1:
        Current = "SimpleTag"
    elif str.find("CompositeTag") != -1:
        Current = "CompositeTag"
    else:
        Current = "EndTag"


def is_end(str):
    global Current
    if Current == "SimpleTag":
        return str.find("</SimpleTag>") != -1
    elif Current == "CompositeTag":
        return str.find("</CompositeTag>") != -1
    else:
        return True


def filter(infile):
    file = open(infile)
    lines = file.readlines()
    file.close()

    txt = []
    #  Copy headers
    txt.append(lines[0][:-1])
    txt.append(lines[1][:-1])

    l = 2
    data = ""

    while l < len(lines):
        v = lines[l][:-1]
        data = data + v.translate(None, ' ')
        set_start(v)
        l = l + 1

        while not is_end(v) and l < len(lines):
            v = lines[l][:-1]
            l = l + 1
            data = data + v.translate(None, ' ')

        txt.append(data)
        data = ""

    return txt.sort()

gprbuild('translations_demo')
run('translations_demo')

res1 = filter("ts.xml")
res2 = filter("tsr.xml")

if res1 == res2:
    print "OK"
else:
    print "NOK"
