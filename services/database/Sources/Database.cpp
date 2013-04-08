#include <vector>
#include <fstream>
#include "Database.h"

JSONNode *Database::CreateTable(const string &name, const vector<string> &columnNames)
{
	if (this->FindTable(name))
		return NULL;
	JSONNode table;
	table.set_name(name);
	JSONNode columns(JSON_ARRAY);
	columns.set_name("columns");
	for (auto i = columnNames.begin(); i != columnNames.end(); i++)
		columns.push_back(JSONNode("", *i));
	table.push_back(columns);
	JSONNode rows(JSON_ARRAY);
	rows.set_name("rows");
	table.push_back(rows);
	auto tables = data.find("tables");
	tables->push_back(table);
	auto t = tables->find(name);
	return t == tables->end() ? NULL : &*t;
}

JSONNode *Database::FindTable(const string &name)
{
	auto tables = data.find("tables");
	auto t = tables->find(name);
	return t == tables->end() ? NULL : &*t;
}

bool Database::DropTable(const string &name)
{
	auto tables = data.find("tables");
	auto t = tables->find(name);
	if (t == tables->end())
		return false;
	tables->erase(t);
}

void Database::Save(ostream &out)
{
	out << data.write_formatted();
}