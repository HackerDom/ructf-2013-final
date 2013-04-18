#include "Utils.h"

class Condition
{
private:
	int mode, operatorMode;
	string operand1, operand2;
	bool operand1IsConstant, operand2IsConstant;
public:
	Condition() { }

	int Compile(JSONNode);

	bool Test(JSONNode, bool);

	void AddCharToFirstOperand(char);

	void AddCharToSecondOperand(char);

	void SetOperation(int);

	void SetMode(int);
};
