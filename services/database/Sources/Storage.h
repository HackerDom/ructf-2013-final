#pragma once

#include <string>
#include <unordered_map>
#include "Database.h"

using namespace std;

#define MAX_BASES 30
#define UID_LENGTH 24
#define KEY_LENGTH 16

class Storage
{
public:
	static Database *CreateDatabase(const string &name, const string &id);
	static Database *FindDatabase(const string &name, bool capture = false);
	static void Initialize();
	static void ReleaseDatabase(const string &name);
	static bool DropDatabase(const string &name);
	static void AddDatabase(const string &name, Database *db);
	static void Flush();
	static void UnloadAll();
	static void SetDbPath(const string &path)
	{
		dbpath = path;
	}
private:
	struct DbInfo
	{
		pthread_mutex_t mutex;
		int lastAccess;
		Database *value;
		DbInfo() : value(NULL), lastAccess(0), mutex(PTHREAD_MUTEX_INITIALIZER) {cout << "everything's empty" << endl; }
		DbInfo(Database *db, int step) : value(db), lastAccess(step), mutex(PTHREAD_MUTEX_INITIALIZER) {cout << "everything's ok" << endl; }
		~DbInfo()
		{
			cout << "NOOO" << endl;
			if (value)
				delete value;
		}
	};
	static int step;
	static pthread_mutex_t mutex;
	static unordered_map<string, DbInfo *> bases;
	static string dbpath;
	static const string signature;
	static void SaveDatabase(const string &name);
	static Database *LoadDatabase(const string &name);
	static void SafeAddDb(const string &name, Database *db);
};