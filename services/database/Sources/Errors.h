#pragma once

#include <string>

using namespace std;

#define ERROR_COUNT 7

enum Errors
{
	ERR_OK = 0,
	ERR_SYNTAX = 1,
	ERR_PERMISSION = 2,
	ERR_NO_DB = 3,
	ERR_NO_TABLE = 4,
	ERR_NO_COLUMN = 5,
	ERR_CORRUPTED = 6
};

string MakeError(int code, const string &msg);

extern string errorStrings[ERROR_COUNT];