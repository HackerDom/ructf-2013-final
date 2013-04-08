#pragma once

#include <string>
#include "libjson/libjson.h"
#include "Database.h"

using namespace std;

class Storage
{
public:
	static Database *CreateDatabase(const string &name, const string &id);
	static Database *FindDatabase(const string &name, const string &id);
	static bool DropDatabase(const string &name, const string &id);
	static void Flush();
	static void UnloadAll();
	static void SetDbPath(const string &path)
	{
		dbpath = path;
	}
private:
	static unordered_map<string, Database *> bases;
	static string dbpath;
	static void SaveDatabase(const string &name);
	static Database *LoadDatabase(const string &name);
};