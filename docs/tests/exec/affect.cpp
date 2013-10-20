#include <iostream>

int main() {
  int x = 0;
  int y = 0;
  x = ++y;
  y = ++y + ++x;
  y++;
  x = --x + y;
  std::cout << "x=" << x << "\n";
  std::cout << "y=" << y << "\n";
}
