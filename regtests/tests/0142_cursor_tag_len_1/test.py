from test_support import *

gprbuild('cursor_tag_l1')
run(['bin/main', '5'])
run(['bin/main', '1'])
