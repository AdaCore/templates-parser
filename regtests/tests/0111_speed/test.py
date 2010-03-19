from test_support import *
import time

gnatmake('speed')

start = time.time()
run('speed')
stop = time.time()

elaps = stop - start

if elaps <= 1:
    print "OK"
else:
    print "NOK: " + str(elaps)
