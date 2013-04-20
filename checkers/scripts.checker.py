#!/usr/bin/env python

import sys
import requests
import random
import socket
import re
import json

# Error codes
OK = 101
CORRUPT = 102
MUMBLE = 103
DOWN = 104
CHECKER_ERROR = 110

def gen_random_str(str_len = 5, abc=list("abcdefgijklmnopqrstuvwxyz1234567890_")):
    ret = []
    for i in range(str_len):
        ret.append(random.choice(abc))
    return "".join(ret)

def register_or_die(host, login, password):
  url = "http://%s/register" % host
  sys.stderr.write("Open %s..\n" % url)
  headers = {'Content-Type': 'application/json', 'X-Requested-With': 'XMLHttpRequest'}
  r = requests.post(url,
                    data=json.dumps({"login": login, "first_name": login, "last_name": login, "password": password, "language": "en"}),
                    headers=headers)
  if r.status_code in [403, 404, 500]:
    sys.stderr.write("Status code %d" % r.status_code)
    print("Failed to register the user in the user service: %s" % login)
    sys.exit(DOWN)
  sys.stderr.write("%s\n" % r.json)

def login_or_die(host, login, password):
  url = "http://%s/login" % host
  sys.stderr.write("Open %s..\n" % url)
  headers = {'Content-Type': 'application/json', 'X-Requested-With': 'XMLHttpRequest'}
  r = requests.post(url,
                    data=json.dumps({"login": login, "password": password}),
                    headers=headers)  
  if "session" not in r.cookies:
    sys.stderr.write("Cookies (without 'session' :( ): %s\n" % r.cookies)
    print("Failed to login to the user service")
    sys.exit(MUMBLE)
  sys.stderr.write("%s\n" % r.json)

  return r.cookies["session"]

def create_user_or_die(host):
  login = gen_random_str(7)
  password = gen_random_str(10)
  register_or_die(host, login, password)
  session = login_or_die(host, login, password)
  return (login, password, session)

def gen_script(flag):
  groups_count = random.randint(5, 10)
  groups = [set() for i in range(groups_count)]
  variables = ['a' + gen_random_str() for i in flag]
  result = ""
  result += '\n'.join(map(lambda x: '%s <- input()' % variables[x], range(len(flag)))) + '\n'
  for ch_id, ch in enumerate(flag):
    if '0' <= ch <= '9':
      idx = random.randint(0, (groups_count - 2) // 2) * 2 + 1
    else:
      idx = random.randint(0, (groups_count - 1) // 2) * 2
    groups[idx].add(ch_id)
  group_variables = {}
  real_values = {}
  for group_id, group in enumerate(groups):
    if len(group) == 0:
      continue
    els = list(group)
    random.shuffle(els)
    group_variables[group_id] = 'a' + gen_random_str()
    if group_id % 2 == 0:
      result += group_variables[group_id] + ' <- ' + ' + '.join(map(lambda x: variables[x], els)) + "\n"
      real_values[group_id] = '"' + ''.join(map(lambda x: flag[x], els)) + '"'
    else:
      result += group_variables[group_id] + ' <- ' + '(' * (len(els) - 1) + ') * 10 + '.join(map(lambda x: variables[x], els)) + "\n"
      real_values[group_id] = int(''.join(map(lambda x: flag[x], els)))
  for group_id, real_value in real_values.items():
    result += 'if %s = %s then\n' % (group_variables[group_id], real_value)
  result += 'print("YES!!" + ' + (' + '.join(variables)) + ')\n'
  result += 'end\n' * len(real_values)
  sys.stderr.write(result)
  return result

def check(host):
  host = "scripts." + host
  
  url = "http://%s/scripts.php" % host
  sys.stderr.write("Open %s..\n" % url)
  r = requests.post(url)
  if r.status_code in [403, 404, 500]:
    sys.stdout.write("Can't open %s, status code 403, 404 or 500\n" % url)
    sys.exit(DOWN)
  if r.status_code != 200:
    sys.stdout.write("Can't open %s, status code %d\n" % (url, r.status_code))
    sys.exit(MUMBLE)
  sys.stderr.write("%s\n" % r)
  sys.exit(OK)

def put(host, flag_id, flag):
  login, password, session = create_user_or_die(host)
  sys.stderr.write(" login=%s\n password=%s\n session=%s\n" % (login, password, session))

  host = "scripts." + host
  url = "http://%s/create.php" % host
  name = gen_random_str(20)
  code = gen_script(flag)
  params = {'save' : 1, 'name' : name, 'code' : code}
  sys.stderr.write("Open %s..\n" % url)
  r = requests.post(url, data=json.dumps(params), cookies={'session': session})

  if r.status_code != 200:
    sys.stderr.write("Status code = %d" % r.status_code)
    sys.exit(DOWN if r.status_code in [403, 404, 500] else MUMBLE)

  print("%s %s %s %s" % (login, password, session, name))

  sys.exit(OK)

def get(host, flag_id, flag):
  login, password, session, name = flag_id.split()
  session = login_or_die(host, login, password)

  host = "scripts." + host
  url = "http://%s/scripts.php" % host
  sys.stderr.write("Open %s..\n" % url)
  r = requests.post(url, data=json.dumps({'json' : 1}), headers={'Content-Type': 'application/json'}, cookies={'session': session})

  if r.status_code != 200:
    sys.stderr.write("Status code = %d\n" % r.status_code)
    sys.exit(DOWN if r.status_code in [403, 404, 500] else MUMBLE)

  sys.stderr.write(str(r.json) + "\n")
  if 'scripts' not in r.json:
    sys.stderr.write("Not found 'scripts', exit..\n")
    sys.stdout.write('Invalid format at /scripts\n')
    sys.exit(MUMBLE)
  scripts = r.json['scripts']
  code_id = ''
  for script in scripts:
    if 'name' not in script:
      continue
    if script['name'] == name:
      if 'code_id' not in script:
        sys.stderr.write('Not found code_id field\n')
        sys.stdout.write('Invalid format at /scripts\n')
        sys.exit(CORRUPT)
      code_id = script['code_id']
  if code_id == '':
    sys.stderr.write("Not found script with name '%s', exit..\n" % name)
    sys.stdout.write('You lost my flag :(\n')
    sys.exit(CORRUPT)

  sys.stderr.write("Code_id = %d\n" % code_id)
  
  url = "http://%s/run.php" % host
  program_input = '\n'.join([x for x in flag])
  r = requests.post(url, data=json.dumps({'code_id' : code_id, 'input': program_input}), headers={'Content-Type': 'application/json'}, cookies={'session': session})
  
  if 'result' not in r.json:
    sys.stderr.write("Not found 'result', exit..\n")
    sys.stdout.write('Invalid response for /run\n')
    sys.exit(MUMBLE)
  result = r.json['result']
  if result != "YES!!" + flag:
    sys.stderr.write("Result not equal to 'YES!!'\n")
    sys.stdout.write('Flag not found at the server\n')
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
    except Exception as e:
        sys.stdout.write('Connection to service failed\n')
        sys.stderr.write("%s\n" % e)
        sys.exit(DOWN)
