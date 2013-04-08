#include "Parser.h"
#include "libjson/libjson.h"

function<int(Query*&, char)> Parser::modifierHandlers[] = {
	[] (Query *& query, char parameter) {
		query->AddCharToDbname(parameter);
		return 0;
	},
	[] (Query *& query, char parameter) {
		query->AddCharToTablename(parameter);
		return 0;
	},
	[] (Query *& query, char parameter) {
		query->AddCharToField(parameter);
		return 0;
	},
	[] (Query *& query, char parameter) {
		query->AddNewField();
		return 0;
	},
	[] (Query *& query, char parameter) {
		query = new CreateDatabaseQuery();
		return 0;
	},
	[] (Query *& query, char parameter) {
		query = new DropDatabaseQuery();
		return 0;
	},
	[] (Query *& query, char parameter) {
		query = new CreateTableQuery();
		return 0;
	},
	[] (Query *& query, char parameter) {
		query = new DropTableQuery();
		return 0;
	},
	[] (Query *& query, char parameter) {
		cout << "Creating select query" << endl;
		query = new SelectQuery();
		cout << "Succefuly created select query" << endl;
		return 0;
	}
};

SymbolAutomaton::SymbolAutomaton(const JSONNode & _syntax)
{
	JSONNode syntax = _syntax.find("nodes")->as_array();
	vector<Node *> mapping(syntax.size(), NULL);
	for (auto j = syntax.begin(); j != syntax.end(); ++j)
		mapping[j->find("id")->as_int()] = new Node();
	for (auto j = syntax.begin(); j != syntax.end(); ++j)
	{
		Node * node = mapping[j->find("id")->as_int()];
		auto modifiers = j->find("modifiers")->as_array();
		for (auto k = modifiers.begin(); k != modifiers.end(); ++k)
			node->modifiers.push_back(k->as_int());
		auto transitions = j->find("transitions")->as_node();
		for (auto k = transitions.begin(); k != transitions.end(); ++k)
		{
			if (k->name().length() == 1)
				node->transitions[k->name()[0]] = mapping[k->as_int()];
			else
			{
				if (k->name() == "alpha")
					for (char c = 'a' ; c <= 'z'; ++c)
						node->transitions[c] = mapping[k->as_int()];
				if (k->name() == "digit")
					for (char c = '0' ; c <= '9'; ++c)
						node->transitions[c] = mapping[k->as_int()];
			}
		}
	}
	initialState = mapping[0];
	currentState = initialState;
}

SymbolAutomaton::~SymbolAutomaton()
{
	delete initialState;
}

vector<int> * SymbolAutomaton::GetModifiers()
{
	if (currentState == NULL)
		return NULL;
	return &currentState->modifiers;
}

void SymbolAutomaton::MoveInitialState(void)
{
	currentState = initialState;
}

bool SymbolAutomaton::MoveNextState(char symbol)
{
	if (currentState == NULL)
	{
		currentState = NULL;
		return false;
	}
	if (currentState->transitions.find(symbol) == currentState->transitions.end())
	{
		currentState = NULL;
		return false;
	}
	else
	{
		currentState = currentState->transitions[symbol];
	}
	return true;
}

SymbolAutomaton * Parser::automaton = NULL;

void Parser::Initialize(const JSONNode & syntax)
{
	automaton = new SymbolAutomaton(syntax);
}

Query * Parser::Parse(const string & queryString, const string & userID)
{
	Query * query = NULL;
	automaton->MoveInitialState();
	cout << "Starting parsing" << endl;
	for (int i = 0; i < queryString.length(); ++i)
	{
		cout << "Trying to move with symbol \"" << queryString[i] << "\"" << endl;
		if (!automaton->MoveNextState(queryString[i]))
			return NULL;
		auto modifiers = automaton->GetModifiers();
		cout << "Got " << modifiers->size() << " modifiers:" << endl;
		if (modifiers->size() != 0)
		{
			for (auto j = modifiers->begin(); j != modifiers->end(); ++j)
				cout << (*j) << ' ';
			cout << endl;
		}
		for (auto j = modifiers->begin(); j != modifiers->end(); ++j)
			modifierHandlers[*j](query, queryString[i]);
	}
	cout << "Parsing complete" << endl;
	if (query != NULL)
		query->SetID(userID);
	return query;
}