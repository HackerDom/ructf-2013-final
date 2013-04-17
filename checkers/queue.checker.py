#!/usr/bin/env python

import sys


# Error codes
OK = 101
CORRUPT = 102
MUMBLE = 103
DOWN = 104
CHECKER_ERROR = 110


def check(host):
    pass


def put(id, flag):
    pass


def get(id, flag):
    pass


if __name__ == "__main__":
    try:
        args = sys.argv[1:]
        if len(args) == 1 and args[0] == "check":
            check(args[0])
        elif len(args) == 3 and args[0] == "put":
            put(args[1], args[2])
        elif len(args) == 3 and args[0] == "get":
            get(args[1], args[2])
        else:
            raise Exception("Wrong arguments")
    except Exception as E:
        sys.stderr.write("{}\n".format(E))
        sys.exit(CHECKER_ERROR)
