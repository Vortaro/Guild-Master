#ifndef ECONOMY_H
#define ECONOMY_H

class Linear
//variables in class are in parenthesis, and the line below demonstrates
//the class 
//y(price)=b(initialprice)-[a(rate)*x(supply)]
//demand curve for "regular" goods
{
private:
  int initialprice;
  float rate;
  int supply;
public:
  void setInitialPrice(int);
  void setRate(float);
  void setSupply(int);
	void resetPrice(short);
  int getInitialPrice() const
	{ return initialprice; }
  float getRate() const
	{ return rate; }
  short getSupply() const
	{ return supply; }
  int getPrice() const  //I don't want any decimals in the final result
	{ return initialprice - (rate * supply); }
};

void Linear::setInitialPrice(int inip)
{
  initialprice = inip;
}
void Linear::setRate(float r)
{
  rate = r;
}
void Linear::setSupply(int supp)
{
  supply = supp;
}

#endif
