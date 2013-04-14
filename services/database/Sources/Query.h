#pragma once

#include "Database.h"
#include "Utils.h"
#include "Storage.h"
#include "Errors.h"

class Query
{
protected:
	string userID;
	string dbname;
	string tablename;
	vector<string> fields;
public:
	virtual int Execute(JSONNode *&) = 0;

	void SetID(const string & _userID) { userID = _userID; }

	void AddCharToDbname(char);

	void AddCharToTablename(char);

	void AddCharToField(char);

	void AddNewField(void);
};

class SelectQuery : public Query
{
private:
		bool godMode;
public:
	SelectQuery(const string & _userID, bool _godMode = false) : godMode(_godMode) { SetID(_userID); }

	SelectQuery(bool _godMode = false) : godMode(_godMode) { }

	int Execute(JSONNode *&);
};

class InsertQuery : public Query
{
public:
	InsertQuery(const string & _userID) { SetID(_userID); }

	InsertQuery() { }

	int Execute(JSONNode *&);
};

class CreateDatabaseQuery : public Query
{
public:
	CreateDatabaseQuery(const string & _userID) { SetID(_userID); }

	CreateDatabaseQuery() { }

	int Execute(JSONNode *&);
};

class CreateTableQuery : public Query
{
public:
	CreateTableQuery(const string & _userID) { SetID(_userID); }

	CreateTableQuery() { }

	int Execute(JSONNode *&);
};

class DropDatabaseQuery : public Query
{
public:
	DropDatabaseQuery(const string & _userID) { SetID(_userID); }

	DropDatabaseQuery() { }

	int Execute(JSONNode *&);
};

class DropTableQuery : public Query
{
public:
	DropTableQuery(const string & _userID) { SetID(_userID); }

	DropTableQuery() { }

	int Execute(JSONNode *&);
};