#include <iostream>

void f(int &y) {
  y = y + 1;
}

int main() {
  int x = 41;
  std::cout << "x = " << x << "\n";
  f(x);
  std::cout << "x = " << x << "\n";
  f(x);
  std::cout << "x = " << x << "\n";
  x = 0;
  std::cout << "x = " << x << "\n";
  f(x);
  std::cout << "x = " << x << "\n";
}
