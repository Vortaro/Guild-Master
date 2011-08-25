#include <iostream>
#include <string>
#include "goods.h"
#include "trade.h"
#include "math.h"
using namespace std;

void trade();
Math abs;
int mquantity; //quantity to buy or sell, m stands for merchant
string chosengood;
long mcost;
long mmoney;
int input;

void goodslist()
{
  cout<<"1. Pottery\n2. Wood\n3. Bricks\n4. Furs\n";
  cin>>input;
  do {
    switch(input) 
      {
      case 1:
				//				chosengood = "pottery";
				pottery();
				trade();
				break;
      case 2:
				trade();
				break;
      case 3:
				trade();
				break;
      case 4:
				trade();
				break;
      default:
				cout << "That's not an option.\n";
				break;
      }
  } while(input > 4);
}

void tradeinterface()
{
  cout << "Welcome to the market.\nAre you interested in buying any goods, or perhaps selling some?\n1. Buy\n2. Sell\n3. Leave the market\n";
  cin >> input;
  do 
    {
      switch(input) 
				{
				case 1:
					goodslist();
					break;
				case 2:
					goodslist();
					break;
				case 3:
					cout << "Come again some time.\n";
					break;
				default:
					cout << "That's not an option.\n";
					break;
				}
    } while(input > 3);
}

void tradeinterfacerep()
{
	cout << "How much money do you have?";
	cin >> mmoney;
	merchant.setMoney(mmoney);
  cout << "1. Buy\n2. Sell\n3. Leave the market\n";
  cin >> input;
  do 
    {
      switch(input) 
				{
				case 1:
					goodslist();
					break;
				case 2:
					goodslist();
					break;
				case 3:
					cout << "Thank you for your business.\n";
					break;
				default:
					cout << "That's not an option.\n";
					break;
				}
    } while(input > 3);
}

void repeattrade()
{
  cout << "Would you like anything else?\n1. Yes\n2. No\n";
  cin >> input;
  switch(input) 
    {
    case 1:
      tradeinterfacerep();
      break;
    case 2:
      cout<< "Thank you for your business.\n";
      break;
    }
}

void enoughmoney()
{
  cout << "If you want "<< merchant.getQuantity() << " crates of " << chosengood << ", that will be " << /*needs function*/ <<" gold coins.\n";
  cout << "Current funds: " << merchant.getMoney() <<"\n";
}
void notenoughmoney()
{
	cout << "It seems that you do not have enough gold coins; consider buying less.\n";  
}

void trade()
{
	cout << "How many crates of "<< chosengood <<" would you like?  ";
	cin >> mquantity;
	merchant.setQuantity(mquantity);
	switch(/*needs function*/) 
		{
		case 0:
			enoughmoney();
			repeattrade(); 
			break;
		case 1:
			notenoughmoney();
			repeattrade();
			break;
		}
}
