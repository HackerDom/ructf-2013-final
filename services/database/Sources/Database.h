#pragma once

#include "Utils.h"
#include "libjson/libjson.h"

using namespace std;

class Database
{
public:
	Database(const JSONNode& node) : data(node) {}
	JSONNode *CreateTable(const string &name, const vector<string> &columns);
	JSONNode *FindTable(const string &name);
	bool DropTable(const string &name);
	void Save(ostream &out);
private:
	JSONNode data;
};