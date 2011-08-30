#include <iostream>
#include <string>

using namespace std;

void market()
{
  cout << "You enter the market.\n";
}
void townhall()
{
  cout << "You enter the town hall.\n";
}
void office()
{
  cout << "You enter your company's office.\n";
}
void tavern()
{
  cout << "You enter the tavern.\n";
}


int city()
{
  int destination;
//  string size;  planned
//  string cityname;  planned

//  cout<<"The "<< size <<" of "<< cityname ;  planned
  cout << "The Town of Suno\n";
  cout << "1. Market\n2. Town Hall\n3. Office\n4. Tavern\n";
  do 
    {
  cout << "Where would you like to go? ";
  cin >> destination;
  switch(destination) 
    {
  case 1:
    market();
    break;
  case 2:
    townhall();
    break;
  case 3:
    office();
    break;
  case 4:
    tavern();
    break;
  default:
    cout<<"That's not an option.\n";
    break;
    }
  } while( destination > 4 );
  return 0;
}
