#!/usr/bin/env python

import socket
import requests
import json
import re

from flask import Flask, request, redirect, url_for , render_template

app = Flask(__name__)

AUTH_SERVER = "127.0.0.1"
QUEUE_SERVER = "127.0.0.1"


#helpers
def get_login_by_session(session):
    ans = requests.post("http://{}/user".format(AUTH_SERVER),
                        headers={"X-Requested-With": "XMLHttpRequest",
                                 "Content-Type": "application/json"},
                        data='{"session": "%s"}' % session)
    try:
        return json.loads(ans.text).get("login")
    except:
        return None


def readline(s):
    chars = []
    while True:
        a = s.recv(1)
        chars.append(a)
        if a == "\n" or a == "":
            return "".join(chars)


def send_cmd_sequence(cmds):
    "Send commands, check for ok status, prints last msg"

    s = socket.create_connection((QUEUE_SERVER, 3255), 3)
    readline(s)

    for cmd in cmds:
        s.send(cmd)
        data = readline(s)
        if not data.startswith("OK"):
            return data
    return data


@app.route("/")
def index():
    return redirect(url_for('list'))


@app.route("/list/")
def list():
    session = request.cookies.get('session', 'none')
    login = get_login_by_session(session)

    raw_result = send_cmd_sequence([b"list\n"])

    queues = []
    error = ""

    if not raw_result.startswith("OK: "):
        error = raw_result
    else:
        queues = re.findall(r"(\w+):(\w+)", raw_result[4:])

    queues = [(user, queue) for user, queue in queues if user == login]
    return render_template('list.html', login=login, queues=queues,
                           error=error)


@app.route("/create/", methods=['POST'])
def create():
    session = request.cookies.get('session', 'none')
    login = get_login_by_session(session)

    queuename = request.form.get('queuename', '')

    cmd_seq = [
        b"user {}\n".format(login),
        b"auth {}\n".format(session),
        b"create {}\n".format(queuename)
    ]

    result = send_cmd_sequence(cmd_seq)
    return render_template('result.html', msg=result)


@app.route("/delete/", methods=['POST'])
def delete():
    session = request.cookies.get('session', 'none')
    login = get_login_by_session(session)

    queuename = request.form.get('queuename', '')

    cmd_seq = [
        b"user {}\n".format(login),
        b"auth {}\n".format(session),
        b"delete {}\n".format(queuename)
    ]

    result = send_cmd_sequence(cmd_seq)
    return render_template('result.html', msg=result)


@app.route("/enqueue/", methods=['POST'])
def enqueue():
    session = request.cookies.get('session', 'none')
    login = get_login_by_session(session)

    queuename = request.form.get('queuename', '')
    val = request.form.get('val', '')

    s = socket.create_connection((QUEUE_SERVER, 3255), 3)
    readline(s)

    cmd_seq = [
        b"user {}\n".format(login),
        b"auth {}\n".format(session),
        b"enqueue {} {}\n".format(queuename, val)
    ]

    result = send_cmd_sequence(cmd_seq)
    return render_template('result.html', msg=result)


@app.route("/dequeue/", methods=['POST'])
def dequeue():
    session = request.cookies.get('session', 'none')
    login = get_login_by_session(session)

    queuename = request.form.get('queuename', '')

    cmd_seq = [
        b"user {}\n".format(login),
        b"auth {}\n".format(session),
        b"dequeue {}\n".format(queuename)
    ]

    result = send_cmd_sequence(cmd_seq)
    return render_template('result.html', msg=result)


if __name__ == "__main__":
    app.debug = False
    app.run(host='0.0.0.0', port=32550)
    app.run()
