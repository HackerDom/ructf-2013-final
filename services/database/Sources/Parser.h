#pragma once

#include "Database.h"
#include "Query.h"
#include "Utils.h"

class SymbolAutomaton
{
private:
	struct Node
	{
		vector<int> modifiers;
		unordered_map<char, Node *> transitions;

		Node() { }

		~Node()
		{
			for (auto j = transitions.begin(); j != transitions.end(); ++j)
				delete j->second;
		}
	};

	Node * initialState, * currentState;
public:
	SymbolAutomaton(const JSONNode & _syntax);

	~SymbolAutomaton();

	vector<int> * GetModifiers();

	bool MoveNextState(char);

	void MoveInitialState();
};

class Parser
{
private:
	static SymbolAutomaton * automaton;

	Parser() { }

	static function<int(Query *&, char)> modifierHandlers[];
public:
	static void Initialize(const JSONNode &);

	static Query * Parse(const string &, const string &);
};