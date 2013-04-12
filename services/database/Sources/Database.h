#pragma once

#include <string>
#include <vector>
#include "libjson/libjson.h"

using namespace std;

class Database
{
public:
	Database(const string &id, const JSONNode &json);
	JSONNode *CreateTable(const string &name, const vector<string> &columns);
	JSONNode *FindTable(const string &name);
	bool DropTable(const string &name);
	string GetId();
	JSONNode *GetJson();
private:
	JSONNode data;
	string uid;
};