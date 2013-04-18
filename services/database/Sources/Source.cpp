#include <signal.h>
#include "libjson/libjson.h"
#include "HttpServer.h"
#include "Errors.h"
#include "Parser.h"
#include "Query.h"

using namespace std;

const string port = "16742";
const string dbpath = "bases";
const string authurl = "127.0.0.1:12345/user";

void TerminationHandler(int) {}

int main(int argc, char **argv)
{
	Storage::Initialize();
	Storage::SetDbPath(dbpath);

	Database *syntaxDb = Storage::FindDatabase(".syntax");
	Parser::Initialize(*syntaxDb->GetJson());

	cout << "Hello and welcome to the Database service." << endl;
	HttpServer server;
	server.SetAuthUrl(authurl);
	server.SetHandler([] (const string &msg, const string &id) {
		JSONNode data = libjson::parse(msg);
		string queryString = data.find("query")->as_string();
		cout << "Query: " << queryString << endl;
		Query *query = Parser::Parse(queryString, id);
		if (query == NULL)
		{
			cout << "Syntax error in query" << endl;
			return errorStrings[ERR_SYNTAX];
		}
		JSONNode *result;
		int errcode = query->Execute(result);
		if (errcode != ERR_OK)
		{
			cout << "Query completed with errors" << endl;
			return errorStrings[errcode];
		}
		cout << "Query completed successfully" << endl;
		result->set_name("data");
		JSONNode n;
		n.push_back(JSONNode("status", "OK"));
		n.push_back(*result);
		delete result;
		delete query;
		return n.write_formatted();
	});
	server.Listen(port);

	if (argc > 1 && string(argv[1]) == "-d")
	{
		cout << "Entering daemon mode" << endl;
		struct sigaction newAction;
		newAction.sa_handler = TerminationHandler;
		sigemptyset(&newAction.sa_mask);
		newAction.sa_flags = 0;
		sigaction(SIGTERM, &newAction, NULL);
		//sigset_t new_mask;
		//sigfillset(&new_mask);
		//sigdelset(&new_mask, SIGCONT);
		//sigsuspend(&new_mask);
		pause();
	}
	else
		cin.get();
	
	server.Stop();

	Storage::Flush();

	return 0;
}
