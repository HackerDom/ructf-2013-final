#pragma once

#include <string>
#include <functional>
#include <mongoose/mongoose.h>

using namespace std;

typedef function<const string(const string &, const string &)> HandlerType;

class HttpServer
{
public:
	void Listen(const string &port);
	void SetHandler(HandlerType handler);
	void SetAuthUrl(const string &url);
private:
	HandlerType handler;
	string authurl;
	int ConnectionHandler(mg_connection *conn);
	void SendJson(mg_connection *conn, const string &json);
	static int RequestHandler(mg_connection *conn);
	string GetId(const string &cookie);
};