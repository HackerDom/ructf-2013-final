#!/usr/bin/python3

import hashlib, sys, json, time, random, string, urllib, os
import urllib.request
from urllib.parse import *

CheckerLogin="DatabaseChecker"
CheckerPassword="DXjAtYeAq6extWEx"

def SendRequest(host, session, requestString):
    data=bytes(json.dumps({ "query" : requestString }), "ASCII")
    request=urllib.request.Request(host, data)
    request.add_header("Cookie", "session=" + session)
    response = json.loads(urllib.request.urlopen(request).readall().decode('ascii'))
    #print(response)
    return response

def CheckCreateDatabase(host, session, databaseName):
    response=SendRequest(host, session, "create database " + databaseName)
    #print(response)
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
    print("Creating database")
    if not CheckCreateDatabase(host, session, databaseName): return False
    print("Successfully created database " + databaseName)

    tableName = GetRandomString(random.randint(20, 40))
    fields = [ GetRandomString(random.randint(20, 40)) ]
    print("Creating table")
    if not CheckCreateTable(host, session, databaseName, tableName, fields): return False

    values = [ GetRandomString(random.randint(20, 40)) ]

    print("Planting values")
    if not CheckInsertValue(host, session, databaseName, tableName, values): return False
    print("Checking planted values")
    if not CheckSelect(host, session, databaseName, tableName, fields, values): return False

    print("Checking drop table")
    if not CheckDropTable(host, session, databaseName, tableName): return False
    print("Checking drop database")
    if not CheckDropDatabase(host, session, databaseName): return False

    return True

def PlantFlag(host, session, tableName, flagID, flag):
    response = SendRequest(host, session, "insert into flags." + tableName + " values ( '" + flagID + "', '" + flag + "')")
    #print("insert into flags." + tableName + " values ( '" + flagID + "', '" + flag + "')")
    #print(response)
    if response["status"] == "FAIL":
        if response["error"]["code"] == 3:
            tResponse = SendRequest(host, session, "create database flags")
            if tResponse["status"] != "OK": return False
        if response["error"]["code"] == 3 or response["error"]["code"] == 4:
            tResponse = SendRequest(host, session, "create table flags." + tableName + " (id, flag)")
            if tResponse["status"] != "OK": return False
        else:
            return False
        response = SendRequest(host, session, "insert into flags." + tableName +" values ( '" + flagID + "', '" + flag + "')")
        if response["status"] != "OK": return False
    return True

def CheckPlantedFlag(host, session, tableName, flagID, flag):
    response = SendRequest(host, session, "select * from flags." + tableName)
    #print("select * from flags." + tableName)
    #print(response)
    if response["status"] == "FAIL" or len(response["data"]["rows"]) < 1: return False
    rows = list(response["data"]["rows"])
    for row in rows:
        if [ row["id"], row["flag"] ] == [ flagID, flag ]:
            return True
    return False

def Authorize(host):
    if True: return "FUCK"
    data=bytes(json.dumps({ "login" : CheckerLogin, "password" : CheckerPassword}), "ASCII")
    request=urllib.request.Request(host + ":12345/login", data)
    request.add_header("X-Requested-With", "XMLHttpRequest")
    request.add_header("Content-Type", "application/json")
    response = urllib.request.urlopen(request)
    responseJson = json.loads(response.readall().decode('ascii'))
    if responseJson["status"] != "OK": 
        if responseJson["error"]["code"] == 3:
            registerData = bytes(json.dumps({ "login" : CheckerLogin, "password" : CheckerPassword, "first_name" : "checker", "last_name" : "checker", "language" : "checker" }), "ASCII")
            #print(registerData)
            registerRequest = urllib.request.Request(host + ":12345/register", registerData)
            registerRequest.add_header("X-Requested-With", "XMLHttpRequest")
            registerRequest.add_header("Content-Type", "application/json")
            registerResponse = json.loads(urllib.request.urlopen(registerRequest).readall().decode('ascii'))
            #print(registerResponse)
            if registerResponse["status"] != "OK":
                print("Login system corrupted")
                exit(110)
            response = urllib.request.urlopen(request).readall()
        else:
            print("Some weird shit is happening with login system")
            exit(110)
    return response.info()["Set-Cookie"].split("session=")[1]


if len(sys.argv) < 3:
    print("Not enough parameters")
    exit(110)

dictFile = open("./DatabaseChecker/Dictionary.txt")
dictionary = dictFile.read().split("\n")
dictFile.close()

flagsTableName = dictionary[(int(time.time() / 60 / 15) * 42167) % len(dictionary)]

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

session = Authorize(CheckerHost)

#print("session is " + session)


if CheckerMode == "check":
    print("Starting checking")
    if not DoCheck(CheckerHost, session):
        print("Something gone wrong with checking")
        exit(103)
    print("Everything working fine")
    exit(101)

if CheckerMode == "put" or CheckerMode == "get":
    if len(sys.argv) < 5:
        print("Flag ID or flag are not supplied")
        exit(110)

    flagID = sys.argv[3]
    flag = sys.argv[4]

    if CheckerMode == "put":
        print("Starting putting flag")
        if not PlantFlag(CheckerHost, session, flagsTableName, flagID, flag):
            print("Something gone wrong")
            exit(103)
        else:
            print("Flag successfully planted")
            plantedFlags[flagID] = flagsTableName
            plantedFlagsFile = open("./DatabaseChecker/" + TeamName , "w")
            plantedFlagsFile.write(json.dumps(plantedFlags, "ASCII"))
            plantedFlagsFile.close()
            exit(101)
    elif CheckerMode == "get":
        print("Starting getting flag")
        if not flagID in plantedFlags:
            print("YOU LIE TO ME, THERE'S NO SUCH FLAG")
            exit(102)
        tableName = plantedFlags[flagID]
        if not CheckPlantedFlag(CheckerHost, session, tableName, flagID, flag):
            print("Something gone wrong, no flag in there")
            exit(102)
        else:
            print("Correct flag detected")
            exit(101)

print("Mode is incorrect")
exit(110)
