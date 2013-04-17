#include "Condition.h"
#include "Errors.h"

int Condition::Compile(JSONNode columns)
{
	if (operand1[0] == '\'')
	{
		operand1IsConstant = true;
		operand1 = operand1.substr(1, operand1.length() - 2);
	}
	else
	{
		operand1IsConstant = false;
		bool contains = false;
		for (auto j = columns.begin(); !contains && j != columns.end(); ++j)
			if (j->as_string() == operand1)
				contains = true;
		if (!contains)
			return Errors::ERR_NO_COLUMN;
	}
	if (operand2[0] == '\'')
	{
		operand2IsConstant = true;
		operand2 = operand2.substr(1, operand2.length() - 2);
	}
	else
	{
		operand2IsConstant = false;
		bool contains = false;
		for (auto j = columns.begin(); !contains && j != columns.end(); ++j)
			if (j->as_string() == operand2)
				contains = true;
		if (!contains)
			return Errors::ERR_NO_COLUMN;
	}
	return Errors::ERR_OK;
}

bool Condition::Test(JSONNode row, bool oldValue)
{
	bool result;
	string operand1Value = operand1IsConstant ? operand1 : row.find(operand1)->as_string();
	string operand2Value = operand2IsConstant ? operand2 : row.find(operand2)->as_string();
	//cout << "operand1 " << operand1Value << endl;
	//cout << "operand2 " << operand2Value << endl;
	//cout << "mode " << (mode == 0 ? "and" : "or") << endl;
	//cout << "operator " << (operatorMode == 0 ? "==" : "!=") << endl;
	switch (operatorMode)
	{
	case 0 :
		result = operand1Value == operand2Value;
		break;
	case 1 :
		result = operand1Value != operand2Value;
		break;
	}
	switch (mode)
	{
	case 0 :
		return oldValue && result;
	case 1 :
		return oldValue || result;
	}
	return false;
}

void Condition::AddCharToFirstOperand(char _char)
{
	operand1.push_back(_char);
}

void Condition::AddCharToSecondOperand(char _char)
{
	operand2.push_back(_char);
}

void Condition::SetOperation(int _operatorMode)
{
	operatorMode = _operatorMode;
}

void Condition::SetMode(int _mode)
{
	mode = _mode;
}
