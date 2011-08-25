#include <iostream>
using namespace std;

int economy();
int city();
void tradeinterface();

int main()
{
  int selection;
  cout << "==============\n"; 
	cout << "*Guild Master*\n";
	cout << "==============\n";
	cout << "1. Trade interface\n2. Demand curve test\n3. City navigation interface\n4. Exit\n";
	cout << "Choose an option.  ";
	cin >> selection;
	switch(selection)
		{
		case 1:
			tradeinterface();
			break;
		case 2:
			economy();
			break;
		case 3:
			city();
			break;
		case 4:
			cout << "Quitting...\n";
			break;
		default:
			cout << "That's not an option.\n";
			break;
		}
	return 0;
}
