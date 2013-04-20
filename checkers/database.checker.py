#!/usr/bin/python3

import hashlib, sys, json, time, random, string, urllib, os
import urllib.request
import urllib.error
import socket
from urllib.parse import *

CheckerLogin="DatabaseChecker"
CheckerPassword="DXjAtYeAq6extWEx"

def SendRequest(host, session, requestString):
    data=bytes(json.dumps({ "query" : requestString }), "ASCII")
    request=urllib.request.Request(host, data)
    request.add_header("Cookie", "session=" + session)
    response = json.loads(urllib.request.urlopen(request).readall().decode('ascii'))
    #sys.stderr.write(response + "\n")
    return response

def CheckCreateDatabase(host, session, databaseName):
    response=SendRequest(host, session, "create database " + databaseName)
    #sys.stderr.write(response + "\n")
    return response["status"] == "OK"

def CheckCreateTable(host, session, databaseName, tableName, fields):
    response=SendRequest(host, session, "create table " + databaseName + "." + tableName + "(" + ",".join(fields) + ")")
    return response["status"] == "OK"

def CheckDropDatabase(host, session, databaseName):
    response=SendRequest(host, session, "drop database " + databaseName)
    return response["status"] == "OK"

def CheckDropTable(host, session, databaseName, tableName):
    response=SendRequest(host, session, "drop table " + databaseName + "." + tableName)
    return response["status"] == "OK"

def CheckInsertValue(host, session, databaseName, tableName, values):
    response=SendRequest(host, session, "insert into " + databaseName + "." + tableName + " values (" + ",".join([ "'" + e + "'" for e in values]) + ")")
    return response["status"] == "OK"

def CheckSelect(host, session, databaseName, tableName, fields, values):
    response=SendRequest(host, session, "select " + ",".join(fields) + " from " + databaseName + "." + tableName)
    if response["status"] != "OK" or len(response["data"]["rows"]) < 1:
        return False

    responseValues = list(response["data"]["rows"][0].values())
    responseValues.sort()
    values.sort()
    return responseValues == values

def GetRandomString(length):
    return "".join([random.choice(string.ascii_letters) for e in range(length)])

def DoCheck(host, session):
    databaseName = GetRandomString(random.randint(20, 40))
    sys.stderr.write("Creating database" + "\n")
    if not CheckCreateDatabase(host, session, databaseName): return False
    sys.stderr.write("Successfully created database " + databaseName + "\n")

    tableName = GetRandomString(random.randint(20, 40))
    fields = [ GetRandomString(random.randint(20, 40)) ]
    sys.stderr.write("Creating table" + "\n")
    if not CheckCreateTable(host, session, databaseName, tableName, fields): return False

    values = [ GetRandomString(random.randint(20, 40)) ]

    sys.stderr.write("Planting values" + "\n")
    if not CheckInsertValue(host, session, databaseName, tableName, values): return False
    sys.stderr.write("Checking planted values" + "\n")
    if not CheckSelect(host, session, databaseName, tableName, fields, values): return False

    sys.stderr.write("Checking drop table" + "\n")
    if not CheckDropTable(host, session, databaseName, tableName): return False
    sys.stderr.write("Checking drop database" + "\n")
    if not CheckDropDatabase(host, session, databaseName): return False

    return True

def PlantFlag(host, session, databaseName, flagID, flag):
    response = SendRequest(host, session, "insert into " + databaseName + ".flags values ( '" + flagID + "', '" + flag + "')")
    #sys.stderr.write("insert into " + databaseName + ".flags values ( '" + flagID + "', '" + flag + "')" + "\n")
    #sys.stderr.write(response + "\n")
    if response["status"] == "FAIL":
        if response["error"]["code"] == 3:
            tResponse = SendRequest(host, session, "create database " + databaseName)
            if tResponse["status"] != "OK": return False
        if response["error"]["code"] == 3 or response["error"]["code"] == 4:
            tResponse = SendRequest(host, session, "create table " + databaseName + ".flags (id, flag)")
            if tResponse["status"] != "OK": return False
        else:
            return False
        response = SendRequest(host, session, "insert into " + databaseName + ".flags values ( '" + flagID + "', '" + flag + "')")
        if response["status"] != "OK": return False
    return True

def CheckPlantedFlag(host, session, databaseName, flagID, flag):
    response = SendRequest(host, session, "select * from " + databaseName + ".flags where id == '" + flagID + "'")
    #sys.stderr.write("select * from " + databaseName + ".flags\n")
    #sys.stderr.write(response + "\n")
    if response["status"] == "FAIL" or len(response["data"]["rows"]) < 1: return False
    rows = list(response["data"]["rows"])
    for row in rows:
        if [ row["id"], row["flag"] ] == [ flagID, flag ]:
            return True
    return False

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
            #sys.stderr.write(registerData + "\n")
            registerRequest = urllib.request.Request(host + ":12345/register", registerData)
            registerRequest.add_header("X-Requested-With", "XMLHttpRequest")
            registerRequest.add_header("Content-Type", "application/json")
            registerResponse = json.loads(urllib.request.urlopen(registerRequest).readall().decode('ascii'))
            #sys.stderr.write(registerResponse + "\n")
            if registerResponse["status"] != "OK":
                sys.stderr.write("Login system corrupted" + "\n")
                exit(110)
            response = urllib.request.urlopen(request).readall()
        else:
            sys.stderr.write("Some weird shit is happening with login system" + "\n")
            exit(110)
    return response.info()["Set-Cookie"].split("session=")[1]


if len(sys.argv) < 3:
    sys.stderr.write("Not enough parameters" + "\n")
    exit(110)

try:
    dictFile = open("./DatabaseChecker/Dictionary.txt")
    dictionary = dictFile.read().split("\n")
    dictFile.close()

    flagsDatabaseName = dictionary[(int(time.time() / 60 / 15) * 42167) % len(dictionary)]

    AuthorizationHost = "http://" + sys.argv[2]
    CheckerHost = "http://db." + sys.argv[2]
    TeamName = sys.argv[2]
    CheckerMode = sys.argv[1]

    if not os.path.exists("./DatabaseChecker/" + TeamName):
        plantedFlagsFile = open("./DatabaseChecker/" + TeamName, "w")
        plantedFlagsFile.close()
        plantedFlags = {}
    else:
        plantedFlagsFile = open("./DatabaseChecker/" + TeamName)
        platedFlagFileContent = plantedFlagsFile.read()
        if len(platedFlagFileContent) == 0:
            plantedFlags = {}
        else:
            plantedFlags = json.loads(platedFlagFileContent)
        plantedFlagsFile.close()

    sys.stderr.write("Authorization..." + "\n")
    session = Authorize(AuthorizationHost)

    #sys.stderr.write("session is " + session + "\n")

    if CheckerMode == "check":
        sys.stderr.write("Starting checking" + "\n")
        if not DoCheck(CheckerHost, session):
            sys.stderr.write("Something gone wrong with checking" + "\n")
            exit(103)
        sys.stderr.write("Everything working fine" + "\n")
        exit(101)

    if CheckerMode == "put" or CheckerMode == "get":
        if len(sys.argv) < 5:
            sys.stderr.write("Flag ID or flag are not supplied" + "\n")
            exit(110)

        flagID = sys.argv[3].replace("-", "")
        print(flagID)
        flag = sys.argv[4]

        if CheckerMode == "put":
            sys.stderr.write("Starting putting flag" + "\n")
            if not PlantFlag(CheckerHost, session, flagsDatabaseName, flagID, flag):
                sys.stderr.write("Something gone wrong" + "\n")
                exit(103)
            else:
                sys.stderr.write("Flag successfully planted" + "\n")
                plantedFlags[flagID] = flagsDatabaseName
                plantedFlagsFile = open("./DatabaseChecker/" + TeamName , "w")
                plantedFlagsFile.write(json.dumps(plantedFlags, "ASCII"))
                plantedFlagsFile.close()
                exit(101)
        elif CheckerMode == "get":
            sys.stderr.write("Starting getting flag" + "\n")
            if not flagID in plantedFlags:
                sys.stderr.write("YOU LIE TO ME, THERE'S NO SUCH FLAG" + "\n")
                exit(102)
            databaseName = plantedFlags[flagID]
            if not CheckPlantedFlag(CheckerHost, session, databaseName, flagID, flag):
                sys.stderr.write("Something gone wrong, no flag ein there" + "\n")
                exit(102)
            else:
                sys.stderr.write("Correct flag detected" + "\n")
                exit(101)

    sys.stderr.write("Mode is incorrect" + "\n")
    exit(110)
except (urllib.error.URLError, socket.gaierror):
    sys.stderr.write("I-net error" + "\n")
    exit(104)
