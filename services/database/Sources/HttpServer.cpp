#include "HttpServer.h"
#include "Errors.h"

using namespace std;

#define unhex(c) ((c) > '9' ? (c) + 10 - ((c) < 'a' ? 'A' : 'a') : (c) - '0')

string UrlDecode(const string &s)
{
	string decoded;
	for (auto i = s.begin(); i < s.end(); i++)
	{
		if (*i != '%')
		{
			decoded.push_back(*i);
			continue;
		}
		if (i + 2 >= s.end())
			return "";
		decoded.push_back(unhex(*(i + 1)) * 16 + unhex(*(i + 2)));
		i += 2;
	}
	return decoded;
}

int HttpServer::RequestHandler(mg_connection *conn)
{
	mg_request_info *ri = mg_get_request_info(conn);
	return ((HttpServer *)ri->user_data)->ConnectionHandler(conn);
}

void HttpServer::Listen(const string &port)
{
	mg_callbacks callbacks;
	memset(&callbacks, 0, sizeof(callbacks));
	callbacks.begin_request = RequestHandler;
	const char *options[] = {
		"listening_ports", port.c_str(), NULL };
	mg_context *ctx = mg_start(&callbacks, this, options);
	cin.get();
	mg_stop(ctx);
}
void HttpServer::SetHandler(HandlerType handler)
{
	this->handler = handler;
}

int HttpServer::ConnectionHandler(mg_connection *conn)
{
	mg_request_info *ri = mg_get_request_info(conn);
	char post_data[4096], data[sizeof(post_data)]; //TODO: do something with length
	int post_data_len = mg_read(conn, post_data, sizeof(post_data));
	mg_get_var(post_data, post_data_len, "data", data, sizeof(data));
	char cookie[64];
	/*if (mg_get_cookie(conn, "session", cookie, sizeof(cookie)) < 0)
	{
		SendJson(conn, errorStrings[ERR_PERMISSION]);
		return 1;
	}*/
	string id = "";//GetId(cookie);
	/*if (id.empty())
	{
		SendJson(conn, errorStrings[ERR_PERMISSION]);
		return 1;
	}*/
	SendJson(conn, handler(UrlDecode(data), id));
	return 1;
}
void HttpServer::SendJson(mg_connection *conn, const string &json)
{
	mg_printf(conn, "HTTP/1.0 200 OK\r\n"
					"Content-Length: %d\r\n"
					"Content-Type: text/plain\r\n\r\n%s", //TODO: application/json?
					json.length(), json.c_str());
}

string HttpServer::GetId(const string &cookie)
{
	return "fgsfds"; //TODO: http client
}