#include <iostream>

int x, *p;

int main() {
  x = 41;
  std::cout << x << "\n";
  p = &x;
  *p = *p + 1;
  std::cout << x << "\n";
  int &r = x;
  r = r + 1;
  std::cout << x << "\n";
}
