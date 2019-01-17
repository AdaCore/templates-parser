"""
This module contains support functions for all test.py
"""

import os
import sys

#  Change directory

TEST = sys.modules['__main__']
TESTDIR = os.path.dirname(TEST.__file__)
TEST_NAME = os.path.basename(TESTDIR)
os.chdir(TESTDIR)

from gnatpython.ex import Run


def gprbuild(prj):
    """Compile a project with gprbuild"""
    cmd = ["gprbuild", "-p", "-P" + prj, "-bargs", "-E"]
    process = Run(cmd)
    if process.status:
        print process.out


def run(bin, options=None, output_file=None):
    """Run a test"""
    if options is None:
        options = []
    if "TIMEOUT" in os.environ:
        timeout = int(os.environ["TIMEOUT"])
    else:
        timeout = 300

    Run([bin] + options, output=output_file, timeout=timeout)
