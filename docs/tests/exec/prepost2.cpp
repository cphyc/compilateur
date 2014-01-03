#include <iostream>

int main() {
  int x = 41;
  std::cout << "x = " << x << "\n";
  std::cout << "x = " << x++ << "\n";
  std::cout << "x = " << ++x << "\n";
  std::cout << "x = " << x-- << "\n";
  std::cout << "x = " << --x << "\n";
  std::cout << "x = " << x << "\n";
}
