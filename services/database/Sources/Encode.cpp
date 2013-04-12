#include <iostream>
#include <string>
#include <fstream>
#include "libjson/libjson.h"
#include "Storage.h"

using namespace std;

int main(int argc, char **argv)
{
	if (argc != 4)
	{
		cout << "Usage:" << endl << "encode <filename> <uid> <output>" << endl;
		return 0;
	}
	Storage::SetDbPath(".");

	ifstream in(argv[1]);
	JSONNode json = libjson::parse(string((std::istreambuf_iterator<char>(in)),
                 std::istreambuf_iterator<char>()));
	in.close();
	cout << json.write_formatted() << endl;

	Database *db = new Database(argv[2], json);
	Storage::AddDatabase(argv[3], db);
	Storage::Flush();
}
