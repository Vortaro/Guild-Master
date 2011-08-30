#ifndef GOODS_H
#define GOODS_H
#include "economy.h"

//This code puts every good on its own demand curve, and are based on the
//general value of the good and its price elasticity, supply is determined
//by cities(whether or not they are produced there and rate of consumption

void pottery()
{
  Linear pottery;
  int potip = 215; //ip stands for initialprice
  float potrate = -.060;
  int potsupply = 80; //I have yet to have production
  pottery.setInitialPrice(potip);
  pottery.setRate(potrate);
  pottery.setSupply(potsupply);
}

#endif
