#!env python3
#
# Cxx Scheme compiler
#
# Copyright © 2020 Alex Kowalenko
#

from glob import glob
from pathlib import Path
import io
import os
import configparser
import tempfile

from colored import fg, bg, attr

import argparse

install_dir = "../../bin"

# Config paramaters
test_cfg = "test.ini"

pre_test = ""
post_test = ""
link_objs = ""
exclude = ""
c_flags = ""

compiler = f"{install_dir}/cxxlisp -s"

local_tempdir = tempfile.gettempdir() + "/"

red = fg('red_1')
restore = attr('reset')


def remove_file(name: str):
    if os.path.isfile(name):
        os.remove(name)

#
# do a compile
#


def do_test(t: str) -> int:
    global llir_compile
    global local_tempdir
    global compiler, c_flags, axlib_dir

    stem = Path(t).stem
    fail = stem + ".fail"
    temp_file = local_tempdir + next(tempfile._get_candidate_names())

    # Compile file
    cmd = f"{compiler} {c_flags} < {t}  > {temp_file} 2>&1 "
    # print(cmd)
    ret = os.system(cmd)
    if ret:
        os.rename(temp_file, fail)
        print(red + "compile " + restore, end="")
        return 0

    # compare
    remove_file(fail)
    exp = stem + ".exp"
    diff_file = local_tempdir + next(tempfile._get_candidate_names())
    cmd = f"diff --ignore-blank-lines {exp} {temp_file} > {diff_file}"
    # print(cmd)
    ret = os.system(cmd)
    if(ret != 0):
        os.rename(temp_file, fail)
    else:
        remove_file(fail)
        os.remove(temp_file)
    os.remove(diff_file)
    if (ret != 0):
        print(red + "parse " + restore, end="")
        return 0
    return 1


#
# do a parse test
#
def do_test_parse(t: str) -> int:
    global local_tempdir
    global compiler, c_flags, axlib_dir

    stem = Path(t).stem
    fail = stem + ".fail"
    temp_file = local_tempdir + next(tempfile._get_candidate_names())

    # Compile file
    cmd = f"{compiler} {c_flags} < {t} > {temp_file}"
    ret = os.system(cmd)
    if ret:
        os.rename(temp_file, fail)
        print(red + "compile " + restore, end="")
        return 0

    # Check output
    exp = stem + ".exp"
    diff_file = local_tempdir + next(tempfile._get_candidate_names())
    cmd = f"diff --ignore-blank-lines {exp} {temp_file} > {diff_file}"
    # print(cmd)
    ret = os.system(cmd)
    if(ret != 0):
        os.rename(temp_file, fail)
    else:
        remove_file(fail)
        os.remove(temp_file)
    os.remove(diff_file)
    if (ret != 0):
        print(red + "parse " + restore, end="")
        return 0
    return 1


# Perform tests on a list of filename
def do_tests(l: list) -> int:
    global exclude
    global pre_test

    # Pre_test
    if(len(pre_test) != 0):
        print("  pre_test")
        compiled_fstring = compile(
            pre_test, '<fstring_from_file>', 'eval')
        formatted_output = eval(compiled_fstring)
        # print(formatted_output)
        os.system(formatted_output)

    count = 0
    fails = 0
    for x in l:
        if x == exclude:
            continue
        print(f"  : {x} ", end="")
        if c_flags.startswith("-p"):
            result = do_test_parse(x)
        else:
            result = do_test(x)
        count += 1
        if(not result):
            print(red + f"-> :Fail" + restore, end="")
            fails += 1
        print()

    if(len(post_test) != 0):
        print("  post_test")
        os.system(post_test)
    print(f"Ran {count} tests with {fails} fails.")

    return fails


# Perform tests on a list of directories
def do_tests_dir(d: list) -> int:
    global pre_test
    global post_test
    global link_objs
    global exclude
    global c_flags

    print(f"Tests: {d}")
    os.chdir(d)

    pre_test = ""
    post_test = ""
    link_objs = ""
    c_flags = ""
    # Read config file if exists
    if os.path.isfile(test_cfg):
        config = configparser.ConfigParser()
        config.read(test_cfg)
        pre_test = config.get("compile", "pre_test", fallback="")
        post_test = config.get("compile", "post_test",  fallback="")
        link_objs = config.get("compile", "link_objs",  fallback="")
        exclude = config.get("compile", "exclude",  fallback="")
        c_flags = config.get("compile", "flags",  fallback="")

    tests = sorted(glob('*.lisp'))
    res = do_tests(tests)
    os.chdir("..")
    return res


def get_tests() -> list:
    tests = glob('tests.*')
    tests.sort()
    return tests


def main() -> int:
    global optimize
    global llir_compile
    global jit

    argsParser = argparse.ArgumentParser()
    argsParser.add_argument(
        '-t', '--tests', help="run test on these directories")
    argsParser.add_argument(
        '-O', '--optimize', help="switch on the optimizer", action='store_true')
    argsParser.add_argument(
        '-l', '--llir', help="compile the .ll output instead", action='store_true')
    argsParser.add_argument(
        '-j', '--jit', help="use the JIT Executor", action='store_true')

    args = argsParser.parse_args()
    if args.optimize:
        print("Optimize:")
        optimize = True

    res = 0
    if args.tests:
        x = args.tests
        tests = x.split(",")
    else:
        tests = get_tests()
    llir_compile = args.llir
    jit = args.jit

    print(f"tests {tests}")
    for d in tests:
        res = res + do_tests_dir(d)
    if res > 0:
        print(red)
    print(f"### Total fails {res}")
    print(restore)
    return res


if __name__ == '__main__':
    exit(main())
