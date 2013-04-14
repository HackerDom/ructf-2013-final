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

	Node * initialState;
public:

	class Iterator
	{
	private:
		Node * currentState;

		Iterator (Node * initState) : currentState(initState) { }

		friend class SymbolAutomaton;
	public:

		bool MoveNextState(char);

		vector<int> * GetModifiers();
	};

	SymbolAutomaton(const JSONNode & _syntax);

	~SymbolAutomaton();

	Iterator GetIterator();
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
