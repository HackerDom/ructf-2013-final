from bottle import route, get, post, run, template, static_file, request, response, redirect, abort
import bottle
import re
import urllib2
import json
import shelve
import subprocess
import signal
import os

import pprint

domain_re = re.compile("(team\d+\.ructf)$")
d = shelve.open('database')
signal.signal(signal.SIGCHLD, signal.SIG_IGN)

@get('/list')
def list():
    domain = re.search(domain_re, request.headers['Host']).group()
    user = get_user(domain)
    if user is None:
        return template('list', proxy=[], user=None, domain=domain)
    uid = str(user['uid'])
    p = []
    if d.has_key(uid):
        u = d[uid]
        for k in u:
            p.append(k)
    return template('list', proxy=p, user=user, domain=domain)

@post('/host/del')
def delete():
    domain = re.search(domain_re, request.headers['Host']).group()
    user = get_user(domain)
    if user is None:
        return abort(404)
    uid = str(user['uid'])
    key = request.forms.get('key')
    if d.has_key(uid):
        u = d[uid]
        if key in u:
            pid = u[key]
            try:
                os.kill(pid, signal.SIGKILL)
            except:
                pass
            del u[key]
            d[uid] = u
    return redirect('/list')

@post('/host/add')
def add():
    domain = re.search(domain_re, request.headers['Host']).group()
    user = get_user(domain)
    if user is None:
        return abort(404)
    uid = str(user['uid'])
    src_port = request.forms.get('src_port')
    dst_host = request.forms.get('dst_host')
    dst_port = request.forms.get('dst_port')
    if src_port and dst_port and dst_port:
        key = '-'.join([src_port, dst_host, dst_port])
        if d.has_key(uid):
            u = d[uid]
            if key in u:
                return abort(404)
            else:
                p = subprocess.Popen(["sleep","10"])
                u[key] = p.pid
                d[uid] = u
                return redirect('/list')
        else:
            p = subprocess.Popen(["sleep","10"])
            d[uid] = {key: p.pid}
            return redirect('/list')
    else:
        print "404!"
        return abort(404)

@route('/static/<filepath:path>')
def server_static(filepath):
    return static_file(filepath, root='./static/files/')

def get_user(domain):
    url = 'http://' + domain + '/user'
    payload = json.dumps({'session': request.get_cookie('session')})
    req = urllib2.Request(url, data=payload, headers={'X-Requested-With': 'XMLHttpRequest', 'Content-Type': 'application/json'})
    try:
        res = urllib2.urlopen(req, timeout=1).read()
        user = json.loads(res)
        if user['status'] == 'OK':
            return user
        else:
            return None
    except:
        return None

run(host='localhost', port=8080)
