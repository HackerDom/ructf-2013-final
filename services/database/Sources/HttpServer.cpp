#include "HttpServer.h"
#include "Errors.h"
#include "libjson/libjson.h"
#include <curl/curl.h>

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
	int contentLength = 0;
	for (int i = 0; i < ri->num_headers; i++)
	{
		if (strcmp("Content-Length", ri->http_headers[i].name))
			continue;
		contentLength = atoi(ri->http_headers[i].value);
	}
	if (contentLength <= 0 || contentLength > 4096)
	{
		SendJson(conn, errorStrings[ERR_SYNTAX]);
		return 1;
	}
	string content(contentLength, 0);
	mg_read(conn, (void *)content.c_str(), contentLength);
	//cout << content << endl;
	char cookie[1024];
	if (mg_get_cookie(conn, "session", cookie, sizeof(cookie)) < 0)
	{
		SendJson(conn, errorStrings[ERR_PERMISSION]);
		return 1;
	}
	string id = GetId(cookie);
	if (id.empty())
	{
		SendJson(conn, errorStrings[ERR_PERMISSION]);
		return 1;
	}
	SendJson(conn, handler(content, id));
	return 1;
}
void HttpServer::SendJson(mg_connection *conn, const string &json)
{
	mg_printf(conn, "HTTP/1.0 200 OK\r\n"
					"Content-Length: %d\r\n"
					"Content-Type: application/json\r\n\r\n%s", //TODO: application/json?
					json.length(), json.c_str());
}

size_t GetCurlResponse(char *ptr, size_t size, size_t nmemb, void *userdata)
{
	((string *)userdata)->append(ptr, ptr + size * nmemb);
	return size * nmemb;
}

void HttpServer::SetAuthUrl(const string &url)
{
	authurl = url;
}

string HttpServer::GetId(const string &cookie)
{
	cout << "getting id" << endl;
	CURL *curl;
	CURLcode res;
 
	//curl_httppost *formpost = NULL;
	//curl_httppost *lastptr = NULL;
	curl_slist *headerlist = NULL;
 
	curl_global_init(CURL_GLOBAL_ALL);
 
	JSONNode n;
	n.push_back(JSONNode("session", cookie));
	//curl_formadd(&formpost, &lastptr, CURLFORM_COPYNAME, "data", CURLFORM_COPYCONTENTS, n.write().c_str(), CURLFORM_END);
	
	headerlist = curl_slist_append(headerlist, "Expect:");
	headerlist = curl_slist_append(headerlist, "X-Requested-With: XMLHttpRequest");
	headerlist = curl_slist_append(headerlist, "Content-Type: application/json");
	curl = curl_easy_init();

	if (curl) 
	{
		curl_easy_setopt(curl, CURLOPT_URL, authurl.c_str());
		curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headerlist);
		//curl_easy_setopt(curl, CURLOPT_HTTPPOST, formpost);

		string response;
		curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, GetCurlResponse);
		curl_easy_setopt(curl, CURLOPT_WRITEDATA, &response);

		string json = n.write_formatted();
		curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json.c_str());
 
		res = curl_easy_perform(curl);
	
		if (res != CURLE_OK)
			curl_easy_strerror(res);
 
		curl_easy_cleanup(curl);
 
		//curl_formfree(formpost);
		curl_slist_free_all (headerlist);

		//cout << response;

		if (!libjson::is_valid(response))
			return "";
		JSONNode auth = libjson::parse(response);
		auto i = auth.find("uid");
		if (i == auth.end())
			return "";
		return i->as_string();
	}
	return "";
}