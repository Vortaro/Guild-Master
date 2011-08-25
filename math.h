#ifndef MATH_H
#define MATH_H
#include "economy.h"
#include "trade.h"
#include "goods.h"

// This code brings aspects of trade.h and economy.h together.  Namely money
//compared to price, and resulting money after purchasing or selling

Merchant merchant;

class Math
{
 private:
	static long cost;
 public:
	void setCost(long);
	friend void setPayment(long);
	friend void setProfit(long);
	long getCost() const
	{ return cost; }
	bool compare() const
	{ return merchant.getMoney() > cost; }
};

void Math::setCost(long c)
{
	cost = c;
}

#endif
