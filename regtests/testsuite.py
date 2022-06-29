#!/usr/bin/env python
"""./testsuite.py [options] [test name]

Run the template-parser testsuite.
"""

from glob import glob
import os
import sys
import time

from e3.env import Env
from e3.os.process import Run
from e3.fs import mkdir, rm
from e3.sys import interpreter

import e3.testsuite
from e3.testsuite import Testsuite
from e3.testsuite.driver.diff import DiffTestDriver
from e3.testsuite.result import Log, TestResult, TestStatus
from e3.testsuite.testcase_finder import ParsedTest, YAMLTestFinder, TestFinder


class BasicTestDriver(DiffTestDriver):
    """Simple test driver that runs a python script inside the TP tests."""

    def run(self):
        """Run the template parser test.

        Executes the test.py command inside the tests and compares the results.
        """
        cmd = [interpreter(), "test.py"]
        start_time = time.time()
        run = self.shell(
            cmd,
            cwd=self.test_env["working_dir"],
            catch_error=False,
            timeout=None,
        )
        self.result.time = time.time() - start_time


class TPTestsuite(Testsuite):
    """Testsuite for Template Parser."""

    test_driver_map = {"basic": BasicTestDriver}
    default_driver = "basic"

    tests_subdir = "tests"

    def __init__(self):
        super().__init__()
        target = os.environ.get("TARGET")
        prj_build = os.environ.get("PRJ_BUILD")

        if target is None:
            target = Run(["gcc", "-dumpmachine"]).out.strip("\n")
        else:
            target = target.lower()

        if prj_build is None:
            prj_build = "debug"
        else:
            prj_build = prj_build.lower()

        def makedir(dir):
            return (
                os.getcwd()
                + "/../.build/"
                + dir
                + "/"
                + target
                + "/"
                + prj_build
                + "/static/"
            )

        env = Env()
        env.add_search_path("PYTHONPATH", os.getcwd())
        env.add_search_path(
            "PATH",
            os.environ.get("PATH")
            + os.pathsep
            + makedir("bin")
            + os.pathsep
            + makedir("rbin"),
        )

    @property
    def test_finders(self):
        return [YAMLTestFinder()]

    def add_options(self, parser):
        parser.add_argument(
            "--diffs",
            dest="view_diffs",
            action="store_true",
            default=False,
            help="Print .diff content",
        )
        parser.add_argument(
            "--old-result-dir",
            dest="old_result_dir",
            type=str,
            default=None,
            help="Old result dir",
        )


if __name__ == "__main__":
    sys.exit(TPTestsuite().testsuite_main())
