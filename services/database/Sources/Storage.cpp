#include <fstream>
#include <sstream>
#include <cstdio>
#include "Storage.h"

string Storage::dbpath = ".";
unordered_map<string, Database *> Storage::bases = unordered_map<string, Database *>();

Database *Storage::CreateDatabase(const string &name, const string &id)
{
	if (FindDatabase(name, id))
		return NULL;
	JSONNode tables(JSON_ARRAY);
	tables.set_name("tables");
	auto db = new Database(tables);
	bases[name] = db;
	SaveDatabase(name);
	return db;
}

Database *Storage::FindDatabase(const string &name, const string &id)
{
	if (bases.find(name) != bases.end())
		return bases[name];
	return LoadDatabase(name);
}

bool Storage::DropDatabase(const string &name, const string &id)
{
	if (bases.find(name) != bases.end())
	{
		delete bases[name];
		bases.erase(name);
	}
	stringstream dbfile;
	dbfile << dbpath << "/" << name;
	remove(dbfile.str().c_str());
	return true;
}

void Storage::SaveDatabase(const string &name)
{
	stringstream dbfile;
	dbfile << dbpath << "/" << name;
	ofstream out(dbfile.str());
	bases[name]->Save(out);
	out.close();
}

Database *Storage::LoadDatabase(const string &name)
{
	stringstream dbfile;
	dbfile << dbpath << "/" << name;
	cout << dbfile.str() << endl;
	ifstream in(dbfile.str());
	if (!in)
		return NULL;
	string s = string((std::istreambuf_iterator<char>(in)),
                 std::istreambuf_iterator<char>());
	cout << "fuck me! " << s << endl;
	JSONNode json = libjson::parse(s);
	cout << json.write_formatted() << endl;
	in.close();
	auto db = new Database(json);
	return bases[name] = db;
}