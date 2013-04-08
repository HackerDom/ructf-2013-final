#include <iostream>
#include <string>
#include <fstream>
#include "libjson/libjson.h"
#include "HttpServer.h"
#include "Errors.h"
#include "Parser.h"
#include "Query.h"

using namespace std;

const string port = "16744";

int main()
{
	//string plaintext = "Hey ho!", key = "qwerty";
	//byte plaintext[100], ciphertext[100], key[Sosemanuk::DEFAULT_KEYLENGTH], iv[Sosemanuk::IV_LENGTH];
 //   Sosemanuk::Encryption enc(key, Sosemanuk::DEFAULT_KEYLENGTH, iv);
 //   enc.ProcessData(ciphertext, plaintext, 100);
 //   Sosemanuk::Decryption dec(key, Sosemanuk::DEFAULT_KEYLENGTH, iv);
 //   dec.ProcessData(plaintext, ciphertext, 100);
	Storage::SetDbPath(".");

	ifstream syntaxFile("Syntax.txt");
	Parser::Initialize(libjson::parse(string((std::istreambuf_iterator<char>(syntaxFile)),
                 std::istreambuf_iterator<char>())));
	syntaxFile.close();

	cout << "Hello and welcome to the Database service." << endl;
	HttpServer server;
	server.SetHandler([] (const string &msg, const string &id) {		
		JSONNode data = libjson::parse(msg);
		Query *query = Parser::Parse(data.find("query")->as_string(), id);
		if (query == NULL)
			return errorStrings[ERR_SYNTAX];
		JSONNode *result;
		int errcode = query->Execute(result);
		cout << "fuck! it is not query!" << endl;
		if (errcode != ERR_OK)
			return errorStrings[errcode];
		result->set_name("data");
		JSONNode n;
		n.push_back(JSONNode("status", "OK"));
		n.push_back(*result);
		return n.write_formatted();
	});
	server.Listen(port);
	return 0;
}