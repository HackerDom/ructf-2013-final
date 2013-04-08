#include "Errors.h"
#include "libjson/libjson.h"

string errorStrings[ERROR_COUNT] = { 
	"",
	MakeError(ERR_SYNTAX, "Invalid query syntax"),
	MakeError(ERR_PERMISSION, "Permission denied"),
	MakeError(ERR_NO_DB, "Database does not exist"),
	MakeError(ERR_NO_TABLE, "Table does not exist"),
	MakeError(ERR_NO_COLUMN, "Column does not exist")
};
string MakeError(int code, const string &msg)
{
	JSONNode n;
	n.push_back(JSONNode("status", "FAIL"));
	JSONNode error;
	error.set_name("error");
	error.push_back(JSONNode("code", code));
	error.push_back(JSONNode("str", msg));
	n.push_back(error);
	return n.write_formatted();
}