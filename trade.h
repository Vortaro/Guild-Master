#ifndef TRADE_H
#define TRADE_H

class Merchant
//Merchant refers to the player, so this class makes objects pertaining
//to him or her
{
 private:
  long money;
  long quantity;
 public:
  void setMoney(long);
  void setQuantity(long);
  long getMoney() const
  { return money; }
  int getQuantity() const
  { return quantity; }
};

void Merchant::setMoney(long m)
{
  money = m;
}
void Merchant::setQuantity(long q)
{
  quantity = q;
}

#endif
