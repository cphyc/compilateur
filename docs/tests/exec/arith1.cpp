#include <iostream>

int main() {
  int x = 41;
  std::cout << "x = " << x << "\n";
  x = x+1;
  std::cout << "x = " << x << "\n";
  x = 2*x;
  std::cout << "x = " << x << "\n";
  x = 2*x + 3;
  std::cout << "x = " << x << "\n";
  x = 1 - 2*x;
  std::cout << "x = " << x << "\n";
}
