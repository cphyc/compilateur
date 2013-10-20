#include <iostream>

int &f(int b, int &x, int &y) {
  std::cout << "x = " << x << "\n";
  std::cout << "y = " << y << "\n";
  if (b) return x; else return y;
}

int main() {
  int x = 42;
  int y = 43;
  int &r = f(1, x, y);
  // now r is an alias for x
  r = 12;
  std::cout << "x = " << x << "\n";
  std::cout << "y = " << y << "\n";
  f(0,x,y) = 13;
  std::cout << "x = " << x << "\n";
  std::cout << "y = " << y << "\n";
}
