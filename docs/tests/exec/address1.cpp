#include <iostream>

int main() {
  int x = 41;
  std::cout << "x = " << x << "\n";
  int *y = &x;
  *y = 42;
  std::cout << "x = " << x << "\n";
  *y = *y + 1;
  std::cout << "x = " << x << "\n";
}
