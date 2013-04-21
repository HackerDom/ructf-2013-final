#!/usr/bin/env python

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
command = "../ips"
try:
    os.mkdir('rules')
except:
    pass

for uid in d.keys():
    u = d[uid];
    for key in u:
        (src_port, dst_host, dst_port) = key.split('-')
        p = subprocess.Popen([command, src_port, dst_host, dst_port, "./rules/" + key])
        u[key] = p.pid
        d[uid] = u

@get('/')
def index():
    domain = re.search(domain_re, request.headers['Host']).group()
    user = get_user()
    return template('index', domain=domain, user=user)

@get('/api_list')
def api_list():
    user = get_user()
    if user is None:
        return json.dumps({'status': "FAIL"})
    uid = str(user['uid'])  
    p = []
    if d.has_key(uid):
        u = d[uid]
        for k in u:
            (src_port, dst_host, dst_port) = k.split('-')
            p.append({"src_port": int(src_port), "dst_host": dst_host, "dst_port": int(dst_port)})
    return json.dumps({'status':"OK", "rules":p})

@get('/list')
def list():
    domain = re.search(domain_re, request.headers['Host']).group()
    user = get_user()
    if user is None:
        return template('list', proxy=[], user=None, domain=domain)
    uid = str(user['uid'])
    p = []
    if d.has_key(uid):
        u = d[uid]
        for k in u:
            p.append(k)
    return template('list', proxy=p, user=user, domain=domain)

@post('/api_del')
def api_delete():
    user = get_user()
    if user is None:
        return json.dumps({'status': "FAIL"})
    uid = str(user['uid'])
    body = json.load(request.body)
    key = '-'.join([str(body["src_port"]), body["dst_host"], str(body["dst_port"])])
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
        else:   
            return json.dumps({'status': "FAIL"})    
    return json.dumps({'status': "OK"})

@post('/host/del')
def delete():
    domain = re.search(domain_re, request.headers['Host']).group()
    user = get_user()
    if user is None:
        return redirect('/')
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

@post('/api_add')
def api_add():
    user = get_user()
    if user is None:
        return json.dumps({'status': "FAIL"})
    uid = str(user['uid'])
    body = json.load(request.body)
    src_port = body["src_port"]
    dst_host = body["dst_host"]
    dst_port = body["dst_port"]
    try:
        if src_port < 60000 or src_port > 65535:
            return json.dumps({'status': "FAIL"})
        if dst_port < 0 or dst_port > 65535:
            return json.dumps({'status': "FAIL"})
    except:
        return json.dumps({'status': "FAIL"})
    src_port = str(src_port)
    dst_port = str(dst_port)
    rules = body["rules"]
    if src_port and dst_port and dst_port:
        key = '-'.join([src_port, dst_host, dst_port])
        if d.has_key(uid):
            u = d[uid]
            if key in u:
                return json.dumps({'status': "FAIL"})
            else:
                p = subprocess.Popen([command, src_port, dst_host, dst_port, "./rules/" + key])
                u[key] = p.pid
                d[uid] = u
                save_rules(key, rules)
                return json.dumps({'status': "OK"})
        else:
            p = subprocess.Popen([command, src_port, dst_host, dst_port, "./rules/" + key])
            d[uid] = {key: p.pid}
            save_rules(key, rules)
            return json.dumps({'status': "OK"})
    else:
        return json.dumps({'status': "FAIL"})

@post('/host/add')
def add():
    domain = re.search(domain_re, request.headers['Host']).group()
    user = get_user()
    if user is None:
        return abort(404)
    uid = str(user['uid'])
    src_port = request.forms.get('src_port')
    dst_host = request.forms.get('dst_host')
    dst_port = request.forms.get('dst_port')
    try:
        port = int(src_port)
        if port < 60000 or port > 65535:
            return abort(404)
        port = int(dst_port)
        if port < 0 or port > 65535:
            return abort(404)
    except:
        return abort(404)
    rules = request.forms.get('rules')
    if src_port and dst_port and dst_port:
        key = '-'.join([src_port, dst_host, dst_port])
        if d.has_key(uid):
            u = d[uid]
            if key in u:
                return abort(404)
            else:
                p = subprocess.Popen([command, src_port, dst_host, dst_port, "./rules/" + key])
                u[key] = p.pid
                d[uid] = u
                save_rules(key, rules)
                return redirect('/list')
        else:
            p = subprocess.Popen([command, src_port, dst_host, dst_port, "./rules/" + key])
            d[uid] = {key: p.pid}
            save_rules(key, rules)
            return redirect('/list')
    else:
        return abort(404)

@route('/static/<filepath:path>')
def server_static(filepath):
    return static_file(filepath, root='./static/files/')

def save_rules(key, rules):
    file = './rules/' + key
    try:
        f = open(file, "w")
        f.write(rules)
        f.close()
    except:
        pass

def get_user():
    url = 'http://127.0.0.1/user'
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

run(host='localhost', port=9000)