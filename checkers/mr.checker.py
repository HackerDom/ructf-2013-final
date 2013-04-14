#!/usr/bin/python3

import hashlib, sys, json, time, random, string, urllib
import urllib.request
from urllib.parse import *

CheckerLogin="MrChecker"
CheckerPassword="Eaa>NfxVfqnhfkfkf"

def GetRandomString(length):
    return "".join([random.choice(string.ascii_letters) for e in range(length)])

def random_matrix(key):
    matrix = []
    for letter in key[:-1]:
        variants = [letter for _ in range(16)]
        variants += [random.choice("".join(set(string.hexdigits) - set(string.ascii_lowercase))) for _ in range(15)]
        random.shuffle(variants)
        matrix.append(variants)
    variants = ['=' for _ in range(16)]
    variants += [random.choice(string.punctuation) for _ in range(15)]
    random.shuffle(variants)
    matrix.append(variants)
    m = map(lambda str: "".join(str), zip(*matrix))
    return dict((random.choice(string.ascii_lowercase), val) for val in m)

def choices(key):
    choices = ['check("{0}", {1}) -> "{0}";'.format(key, pos + 1) for (pos, key) in enumerate(key)]
    random.shuffle(choices)
    choices.append('check(_, _) -> "x".')
    return "\n".join(choices)

def checker_code(key):
    return open('mr/check.erl', "r").read() + choices(key)

def example_code():
    return open('mr/example.erl', 'r').read()

def example_data():
    return {"1": "foo", "2": "foo", "3": "bar", "4": "foo", "5": "buzz", "6": "bar"}

def upload(host, session, name, code):
    data=bytes(json.dumps({ "name" : name, "code" : code }), "ASCII")
    request=urllib.request.Request(host + ":8080/upload", data)
    request.add_header("Content-Type", "application/json")
    request.add_header("Cookie", "session=" + session)
    response = urllib.request.urlopen(request)
    responseJson = json.loads(response.readall().decode('ascii'))
    return responseJson["status"] == "OK"

def mr_exec(host, session, name, data):
    request_json =bytes(json.dumps({ "name" : name, "data" : data }), "ASCII")
    request=urllib.request.Request(host + ":8080/exec", request_json)
    request.add_header("Content-Type", "application/json")
    request.add_header("Cookie", "session=" + session)
    response = urllib.request.urlopen(request)
    responseJson = json.loads(response.readall().decode('ascii'))
    if responseJson["status"] != "OK": 
        return False
    else:
        return responseJson["result"] 


def do_check(host, session):
    name = GetRandomString(random.randint(20, 40))
    sys.stderr.write("Uploading script"+"\n")
    if not upload(host, session, name, example_code()): return False
    sys.stderr.write("Uploaded " + name+"\n")

    sys.stderr.write("Executing"+"\n")
    result = mr_exec(host, session, name, example_data())
    sys.stderr.write("Got {0}".format(result)+"\n")
    if not result or not "buzzbarfoo" == "".join(sorted(result, key=lambda str: int(result[str][0]))): 
        return False

    sys.stderr.write("Success"+"\n")

    return True

def do_put(host, session, name, flag):
    sys.stderr.write("Uploading script"+"\n")
    if not upload(host, session, name, checker_code(flag)): return False
    sys.stderr.write("Uploaded " + name+"\n")
    return True

def do_exec(host, session, name, flag):
    sys.stderr.write("Executing"+"\n")
    result = mr_exec(host, session, name, random_matrix(flag))
    if not result or not flag == "".join([result[str(l)][0] for l in range(1,33)]): 
        return False
    sys.stderr.write("Success"+"\n")

    return True

def Authorize(host):
    data=bytes(json.dumps({ "login" : CheckerLogin, "password" : CheckerPassword}), "ASCII")
    request=urllib.request.Request(host + "/login", data)
    request.add_header("X-Requested-With", "XMLHttpRequest")
    request.add_header("Content-Type", "application/json")
    response = urllib.request.urlopen(request)
    responseJson = json.loads(response.readall().decode('ascii'))
    if responseJson["status"] != "OK": 
        if responseJson["error"]["code"] == 3:
            registerData = bytes(json.dumps({ "login" : CheckerLogin, "password" : CheckerPassword, "first_name" : "checker", "last_name" : "checker", "language" : "checker" }), "ASCII")
            registerRequest = urllib.request.Request(host + "/register", registerData)
            registerRequest.add_header("X-Requested-With", "XMLHttpRequest")
            registerRequest.add_header("Content-Type", "application/json")
            registerResponse = json.loads(urllib.request.urlopen(registerRequest).readall().decode('ascii'))
            if registerResponse["status"] != "OK":
                sys.stderr.write("Login system corrupted" + "\n")
                exit(110)
            response = urllib.request.urlopen(request)
        else:
            sys.stderr.write("Some weird shit is happening with login system" + "\n")
            exit(110)
    return response.info()["Set-Cookie"].split("session=")[1]

if len(sys.argv) < 3:
    sys.stderr.write("Not enough parameters"+"\n")
    exit(110)

try:
    CheckerHost="http://" + sys.argv[2]
    session = Authorize(CheckerHost)

    sys.stderr.write("session is " + session+"\n")

    if sys.argv[1] == "check":
        sys.stderr.write("Starting checking"+"\n")
        if not do_check(CheckerHost, session):
            sys.stderr.write("Something gone wrong with checking"+"\n")
            exit(104)
        sys.stderr.write("Everything working fine"+"\n")
        exit(101)
    elif sys.argv[1] == "put":
        if len(sys.argv) < 5: exit(110)
        sys.stderr.write("Starting putting flag"+"\n")
        if not do_put(CheckerHost, session, sys.argv[3], sys.argv[4]):
            sys.stderr.write("Something gone wrong"+"\n")
            exit(104)
        else:
            sys.stderr.write("Flag successfully planted"+"\n")
            exit(101)
    elif sys.argv[1] == "get":
        if len(sys.argv) < 5: exit(110)
        sys.stderr.write("Starting getting flag"+"\n")
        if not do_exec(CheckerHost, session, sys.argv[3], sys.argv[4]):
            sys.stderr.write("Something gone wrong"+"\n")
            exit(104)
        else:
            sys.stderr.write("Correct flag detected"+"\n")
            exit(101)
    else:
        sys.stderr.write("Mode is incorrect"+"\n")
        exit(110)
except:
    exit(104)
