#include <iostream>

void f(int &y) {
  y = 42;
}

int main() {
  int x = 41;
  std::cout << "x = " << x << "\n";
  f(x);
  std::cout << "x = " << x << "\n";
  x = 41;
  std::cout << "x = " << x << "\n";
}
