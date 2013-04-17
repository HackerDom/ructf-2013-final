#!/usr/bin/env python

import sys
import requests
import random
import socket
import sha
import re


PORT = 3255

# Error codes
OK = 101
CORRUPT = 102
MUMBLE = 103
DOWN = 104
CHECKER_ERROR = 110


# Helpers
def gen_random_str(str_len, abc=list("abcdefgijklmnopqrstuvwxyz1234567890")):
    ret = []
    for i in range(str_len):
        ret.append(random.choice(abc))
    return "".join(ret)


def gen_secret_hash(s):
    return sha.new("VerySecretString" + s + "MoreSecretness").hexdigest()[:20]


def gen_another_secret_hash(s):
    return sha.new("AnotherString" + s + "MoreSecretness").hexdigest()[:20]


def readline(s):
    chars = []
    while True:
        a = s.recv(1)
        chars.append(a)
        if a == "\n" or a == "":
            return "".join(chars)


def send_and_check_for_output(s, line, substr, stage, fail_exit_code=MUMBLE):
    s.send(line)
    ans = readline(s)
    if substr not in ans:
        print('{}: bad output'.format(stage))
        sys.exit(fail_exit_code)


def register_or_die(host, login, password):
    ans = requests.post("http://{0}/register".format(host),
                        data={"login": login, "first_name": login,
                              "last_name": "", "password": password,
                              "language": "en"})
    if ans.status_code != 200:
        print("Failed to register the user in the user service: %s, %s, %s" % (login, host, ans.status_code))
        sys.exit(DOWN)


def get_session_num_or_die(host, login, password):
    ans = requests.post("http://{0}/login".format(host),
                        data={"login": login, "password": password})

    if "session" not in ans.cookies:
        print("Failed to login to the user service")
        sys.exit(DOWN)

    return ans.cookies["session"]


def check(host):
    user = gen_random_str(10)
    password = gen_random_str(10)

    register_or_die(host, user, password)
    session = get_session_num_or_die(host, user, password)

    try:
        s = socket.create_connection((host, PORT), 10)
    except Exception as E:
        print('Connect: {}'.format(E))
        sys.exit(OK)

    readline(s)

    send_and_check_for_output(s, b"user {}\n".format(user),
                              "OK", "USER cmd")
    send_and_check_for_output(s, b"auth {}\n".format(session),
                              "OK", "AUTH cmd")

    queuename = gen_random_str(10)

    send_and_check_for_output(s, b"create {}\n".format(queuename),
                              "OK", "CREATE cmd")

    first = gen_random_str(10)
    second = gen_random_str(10)
    third = gen_random_str(10)

    send_and_check_for_output(s, b"enqueue {} {}\n".format(queuename, first),
                              "OK", "ENQUEUE cmd")
    send_and_check_for_output(s, b"dequeue {}\n".format(queuename),
                              first, "DEQUEUE cmd")

    send_and_check_for_output(s, b"enqueue {} {}\n".format(queuename, second),
                              "OK", "ENQUEUE cmd")
    send_and_check_for_output(s, b"enqueue {} {}\n".format(queuename, third),
                              "OK", "ENQUEUE cmd")

    send_and_check_for_output(s, b"dequeue {}\n".format(queuename),
                              second, "DEQUEUE cmd")
    send_and_check_for_output(s, b"dequeue {}\n".format(queuename),
                              third, "DEQUEUE cmd")
    send_and_check_for_output(s, b"dequeue {}\n".format(queuename),
                              "ERR", "DEQUEUE cmd")

    send_and_check_for_output(s, b"list\n", queuename, "LIST cmd")
    send_and_check_for_output(s, b"list\n", user, "LIST cmd")

    send_and_check_for_output(s, b"delete {}\n".format(queuename),
                              "OK", "DELETE cmd")
    send_and_check_for_output(s, b"delete {}\n".format(queuename),
                              "ERR", "DELETE cmd")

    sys.exit(OK)


def put(host, flag_id, flag):
    user = flag_id.replace("-", "")
    password = gen_secret_hash(user)

    register_or_die(host, user, password)
    session = get_session_num_or_die(host, user, password)

    try:
        s = socket.create_connection((host, PORT), 10)
    except Exception as E:
        print('Connect: {}'.format(E))
        sys.exit(OK)

    readline(s)

    send_and_check_for_output(s, b"user {}\n".format(user),
                              "OK", "USER cmd")
    send_and_check_for_output(s, b"auth {}\n".format(session),
                              "OK", "AUTH cmd")

    queuename = gen_another_secret_hash(user)
    send_and_check_for_output(s, b"create {}\n".format(queuename),
                              "OK", "CREATE cmd")
    send_and_check_for_output(s, b"enqueue {} {}\n".format(queuename, flag),
                              "OK", "ENQUEUE cmd")
    sys.exit(OK)


def get(host, flag_id, flag):
    user = flag_id.replace("-", "")
    password = gen_secret_hash(user)

    session = get_session_num_or_die(host, user, password)

    try:
        s = socket.create_connection((host, PORT), 10)
    except Exception as E:
        print('Connect: {}'.format(E))
        sys.exit(OK)

    readline(s)

    send_and_check_for_output(s, b"user {}\n".format(user),
                              "OK", "USER cmd")
    send_and_check_for_output(s, b"auth {}\n".format(session),
                              "OK", "AUTH cmd")

    queuename = gen_another_secret_hash(user)

    s.send(b"dequeue {}\n".format(queuename))
    ans = readline(s)

    m = re.match(r"OK: (.+)", ans)
    if not m:
        print("DEQUEUE cmd: bad output")
        sys.exit(CORRUPT)

    my_flag = m.group(1)

    send_and_check_for_output(s, b"enqueue {} {}\n".format(queuename, my_flag),
                              "OK", "ENQUEUE cmd")

    if my_flag != flag:
        print("Stored flag is wrong")
        sys.exit(CORRUPT)

    sys.exit(OK)


if __name__ == "__main__":
    try:
        args = sys.argv[1:]
        if len(args) == 2 and args[0] == "check":
            check(args[1])
        elif len(args) == 4 and args[0] == "put":
            put(args[1], args[2], args[3])
        elif len(args) == 4 and args[0] == "get":
            get(args[1], args[2], args[3])
        else:
            raise Exception("Wrong arguments")
    except Exception as E:
        sys.stderr.write("{}\n".format(E))
        sys.exit(CHECKER_ERROR)
