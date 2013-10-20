#include <iostream>

int main() {
  int x = 41;
  int &y = x;
  y = 42;
  std::cout << "x = " << x << "\n";
}
