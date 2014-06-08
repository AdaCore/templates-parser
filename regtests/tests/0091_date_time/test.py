from test_support import *
import datetime
import time

def check(name, current, expected):
    if current == expected:
        print "OK " + name
    else:
        print "NOK " + name + ": '" + expected + "' != '" + current + "'"

def check_n(name, current, expected, shift=1):
    v = int (current)
    if v == expected or (v > expected - shift and v < expected + shift):
        print "OK " + name
    else:
        print "NOK " + name + ": '" + str(expected) + "' != '" + current + "'"

WeekDays = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
           "Saturday", "Sunday"]

Months = ["January", "February", "March", "April", "May", "June", "July",
          "August", "September", "October", "November", "December"]

now = datetime.datetime.now()

#  Make sure we are not going to change minute in the middle of the test

while now.second > 56:
    time.sleep(5)
    now = datetime.datetime.now()

run('testme', ["testme.tmplt"], output_file="dt.out")
run('print_tree', ["testme.tmplt"])

l1 = WeekDays[now.weekday()] + " " + Months[now.month - 1]
l2 = now.strftime ("%Y/%m/%d")
l3 = now.hour
l4 = now.minute
l5 = now.second

#  Now check dt.out result

file = open ('dt.out')
lines = file.readlines()

check ("L1", lines[0][:-1], l1)
check ("L2", lines[1][:-1], l2)
check_n ("L3", lines[2][:-1], l3)
check_n ("L4", lines[3][:-1], l4)
check_n ("L5", lines[4][:-1], l5, 5)

file.close()
