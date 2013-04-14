#include "Query.h"

int SelectQuery::Execute(JSONNode *& result)
{
	result = new JSONNode();
	Database * db = Storage::FindDatabase(dbname);
	if (db == NULL)
		return Errors::ERR_NO_DB;
	cout << "God mode: " << (godMode ? "on" : "off") << endl;
	if (!godMode && db->GetId() != userID)
		return Errors::ERR_PERMISSION;
	JSONNode * table = db->FindTable(tablename);
	if (table == NULL)
		return Errors::ERR_NO_TABLE;
	JSONNode tableColumns = table->find("columns")->as_array();
	vector<string> columns;
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
				return Errors::ERR_NO_COLUMN;
			columns.push_back(*j);
		}
	}

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

	JSONNode responseColumns(JSON_ARRAY);
	responseColumns.set_name("columns");
	for (auto j = columns.begin(); j != columns.end(); ++j)
		responseColumns.push_back(JSONNode("", *j));

	result->push_back(responseColumns);
	result->push_back(rows);
	return Errors::ERR_OK;
}

int InsertQuery::Execute(JSONNode *& result)
{
	result = new JSONNode();
	Database * db = Storage::FindDatabase(dbname, true);
	if (db == NULL)
		return Errors::ERR_NO_DB;
	if (db->GetId() != userID)
	{
		Storage::ReleaseDatabase(dbname);
		return Errors::ERR_PERMISSION;
	}

	//cout << "Found database" << endl;

	JSONNode * table = db->FindTable(tablename);
	if (table == NULL)
	{
		Storage::ReleaseDatabase(dbname);
		return Errors::ERR_NO_TABLE;
	}

	//cout << "Found table" << endl;

	//cout << "Table has " << table->find("columns")->as_array().size() << " columns" << endl;
	//cout << "We got " << fields.size() << " values" << endl;

	if (table->find("columns")->as_array().size() != fields.size())
	{
		Storage::ReleaseDatabase(dbname);
		return Errors::ERR_SYNTAX;
	}
	JSONNode newRow;
	JSONNode tableColumns = table->find("columns")->as_array();
	auto k = tableColumns.begin();
	for (auto j = fields.begin(); j != fields.end(); ++j, ++k)
	{
		JSONNode node(k->as_string(), *j);
		newRow.push_back(node);
	}

	//cout << "Formed new row" << endl;

	table->find("rows")->push_back(newRow);
	//cout << table->write_formatted() << endl;
	//cout << db->FindTable(tablename)->write_formatted() << endl;
	cout << "Releasing" << endl;

	Storage::ReleaseDatabase(dbname);
	return Errors::ERR_OK;
}

int CreateDatabaseQuery::Execute(JSONNode *& result)
{
	result = new JSONNode();
	Database * db = Storage::CreateDatabase(dbname, userID);
	if (db == NULL)
		return Errors::ERR_PERMISSION;
	return Errors::ERR_OK;
}

int CreateTableQuery::Execute(JSONNode *& result)
{
	result = new JSONNode();
	if (fields.size() == 0)
		return Errors::ERR_SYNTAX;
	unordered_set<string> columnsSet;
	for (auto j = fields.begin(); j != fields.end(); ++j)
		if (columnsSet.find(*j) != columnsSet.end())
			return Errors::ERR_SYNTAX;
		else
			columnsSet.insert(*j);
	Database * db = Storage::FindDatabase(dbname, true);
	if (db == NULL)
		return Errors::ERR_NO_DB;
	if (db->GetId() != userID)
	{
		Storage::ReleaseDatabase(dbname);
		return Errors::ERR_PERMISSION;
	}
	JSONNode * table = db->CreateTable(tablename, fields);
	if (table == NULL)
	{
		Storage::ReleaseDatabase(dbname);
		return Errors::ERR_PERMISSION;
	}
	Storage::ReleaseDatabase(dbname);
	return Errors::ERR_OK;
}

int DropDatabaseQuery::Execute(JSONNode *& result)
{
	result = new JSONNode();
	Database * db = Storage::FindDatabase(dbname);
	if (db == NULL)
		return Errors::ERR_NO_DB;
	if (db->GetId() != userID)
		return Errors::ERR_PERMISSION;
	if (Storage::DropDatabase(dbname))
		return Errors::ERR_OK;
	else
		return Errors::ERR_PERMISSION;
}

int DropTableQuery::Execute(JSONNode *& result)
{
	result = new JSONNode();
	Database * db = Storage::FindDatabase(dbname, true);
	int errcode = ERR_PERMISSION;
	if (db == NULL)
		errcode = Errors::ERR_NO_DB;
	else if (db->GetId() != userID)
		errcode = Errors::ERR_PERMISSION;
	else if (db->FindTable(tablename) == NULL)
		errcode = Errors::ERR_NO_TABLE;
	else if (db->DropTable(tablename))
		errcode = Errors::ERR_OK;

	Storage::ReleaseDatabase(dbname);
	return errcode;
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