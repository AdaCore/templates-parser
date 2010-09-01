from test_support import *

run('print_tree', ["-M", "macro.tmplt"])
run('testme', ["macro.tmplt"])
