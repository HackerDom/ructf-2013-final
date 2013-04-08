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
	void InitFields();
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
public:
	SelectQuery(const string & _userID) { SetID(_userID); InitFields(); }

	SelectQuery() {  InitFields(); }

	int Execute(JSONNode *&);
};

class CreateDatabaseQuery : public Query
{
public:
	CreateDatabaseQuery(const string & _userID) { SetID(_userID); InitFields(); }

	CreateDatabaseQuery() { InitFields(); }

	int Execute(JSONNode *&);
};

class CreateTableQuery : public Query
{
public:
	CreateTableQuery(const string & _userID) { SetID(_userID); InitFields(); }

	CreateTableQuery() { InitFields(); }

	int Execute(JSONNode *&);
};

class DropDatabaseQuery : public Query
{
public:
	DropDatabaseQuery(const string & _userID) { SetID(_userID); InitFields(); }

	DropDatabaseQuery() { InitFields(); }

	int Execute(JSONNode *&);
};

class DropTableQuery : public Query
{
public:
	DropTableQuery(const string & _userID) { SetID(_userID); InitFields(); }

	DropTableQuery() { InitFields(); }

	int Execute(JSONNode *&);
};