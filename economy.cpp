#include <iostream>
#include "economy.h"
using namespace std;
//This is a test program for a linear demand curve
int economy()
{
  Linear good;
  int linnumber;  //lin stands for linear
  float lindecimal;
  cout << "What is the price of the good when supply is zero?  ";
  //Get the initial price (y-intercept) and write it to the good object
  cin >> linnumber;
  good.setInitialPrice(linnumber);
  cout << "At what rate will the price change with each increase by 1\nin supply?  ";
  //Same thing, but for rate of change (slope)
  cin >> lindecimal;
  good.setRate(lindecimal);
  cout << "What is the supply of the good?  ";
  //Also the same, but for supply (x)
  cin >> linnumber;
  good.setSupply(linnumber);
  //Calculate and output the price of the good (y)
  cout << "With " << good.getSupply() << " in stock, the price of the first good bought will be "<< good.getPrice() << ".\n";
  return 0;
}
