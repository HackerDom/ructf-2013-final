#include "Query.h"

int SelectQuery::Execute(JSONNode *& result)
{
	cout << "SelectQuery::Execute started" << endl;
	Database * db = Storage::FindDatabase(dbname, userID);
	if (db == NULL)
	{
		cout << "SelectQuery::Execute ended ERR_NO_DB" << endl;
		return Errors::ERR_NO_DB;
	}
	JSONNode * table = db->FindTable(tablename);
	if (table == NULL)
	{
		cout << "SelectQuery::Execute ended ERR_NO_TABLE" << endl;
		return Errors::ERR_NO_TABLE;
	}
	JSONNode tableColumns = table->find("columns")->as_array();
	cout << tableColumns.write_formatted() << endl;
	vector<string> columns;
	cout << fields.size() << endl;
	for (auto j = fields.begin(); j != fields.end(); ++j)
	{
		if ((*j) == "*")
		{
			for (auto k = tableColumns.begin(); k != tableColumns.end(); ++k)
				columns.push_back(k->as_string());
		}
		else
		{
			bool found = 0;
			for (auto k = tableColumns.begin(); k != tableColumns.end() && !found; ++k)
				if ((*j) == k->as_string())
					found = true;
			if (!found)
			{
				cout << "SelectQuery::Execute ended ERR_NO_COLUMN" << endl;
				return Errors::ERR_NO_COLUMN;
			}
			columns.push_back(*j);
		}
	}
	cout << "Proceeded to rows" << endl;
	JSONNode rows(JSON_ARRAY);
	rows.set_name("rows");
	JSONNode tableRows = table->find("rows")->as_array();
	for (auto j = tableRows.begin(); j != tableRows.end(); ++j)
	{
		JSONNode node;
		for (auto k = columns.begin(); k != columns.end(); ++k)
			node.push_back(JSONNode(*k, j->find(*k)->as_string()));
		rows.push_back(node);	
	}
	cout << "Proceeded to responseColumns" << endl;
	JSONNode responseColumns(JSON_ARRAY);
	responseColumns.set_name("columns");
	for (auto j = columns.begin(); j != columns.end(); ++j)
		responseColumns.push_back(JSONNode("", *j));

	result = new JSONNode();
	result->push_back(responseColumns);
	result->push_back(rows);
	cout << "SelectQuery::Execute ended ERR_OK" << endl;
	return Errors::ERR_OK;
}

int CreateDatabaseQuery::Execute(JSONNode *& result)
{
	Database * db = Storage::CreateDatabase(dbname, userID);
	if (db == NULL)
		return Errors::ERR_PERMISSION;
	return Errors::ERR_OK;
}

int CreateTableQuery::Execute(JSONNode *& result)
{
	unordered_set<string> columnsSet;
	for (auto j = fields.begin(); j != fields.end(); ++j)
		if (columnsSet.find(*j) != columnsSet.end())
			return Errors::ERR_SYNTAX;
		else
			columnsSet.insert(*j);
	Database * db = Storage::FindDatabase(dbname, userID);
	if (db == NULL)
		return Errors::ERR_NO_DB;
	JSONNode * table = db->CreateTable(tablename, fields);
	if (table == NULL)
		return Errors::ERR_PERMISSION;
	return Errors::ERR_OK;
}

int DropDatabaseQuery::Execute(JSONNode *& result)
{
	if (Storage::FindDatabase(dbname, userID) == NULL)
		return Errors::ERR_NO_DB;
	if (Storage::DropDatabase(dbname, userID))
		return Errors::ERR_OK;
	else
		return Errors::ERR_PERMISSION;
}

int DropTableQuery::Execute(JSONNode *& result)
{
	Database * db = Storage::FindDatabase(dbname, userID);
	if (db == NULL)
		return Errors::ERR_NO_DB;
	if (db->FindTable(tablename) == NULL)
		return Errors::ERR_NO_TABLE;
	if (db->DropTable(tablename))
		return Errors::ERR_OK;
	else
		return Errors::ERR_PERMISSION;
}

void Query::AddCharToField(char charToAdd)
{
	if (fields.size() == 0)
		fields.push_back("");
	fields.back().push_back(charToAdd);
}

void Query::AddNewField()
{
	fields.push_back("");
}

void Query::AddCharToDbname(char charToAdd)
{
	dbname.push_back(charToAdd);
}

void Query::AddCharToTablename(char charToAdd)
{
	tablename.push_back(charToAdd);
}

void Query::InitFields() //TODO:delete this shit
{
	dbname = "";
	tablename = "";
	fields = vector<string>();
}