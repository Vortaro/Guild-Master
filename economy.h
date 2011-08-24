#include <iostream>
using namespace std;

class Linear
/*variables in class are in parenthesis, and the line below demonstrates*/
/*the class*/ 
/*y(price)=b(initialprice)-[a(rate)*x(supply)]*/
/*supply curve for "regular" goods*/
{
private:
  int initialprice;
  float rate;
  short supply;
public:
  void setInitialPrice(int);
  void setRate(float);
  void setSupply(short);
  int getInitialPrice() const;
  float getRate() const;
  short getSupply() const;
  int getPrice() const;  //I don't want any decimals in the final result
};

void Linear::setInitialPrice(int inip)
{
  initialprice = inip;
}
void Linear::setRate(float r)
{
  rate = r;
}
void Linear::setSupply(short supp)
{
  supply = supp;
}
int Linear::getInitialPrice() const
{
  return initialprice;
}
float Linear::getRate() const
{
  return rate;
}
short Linear::getSupply() const
{
  return supply;
}
int Linear::getPrice() const
{
  return initialprice - (rate * supply);
}
