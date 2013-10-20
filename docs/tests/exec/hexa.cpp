#include <iostream>

int main() {
  std::cout << 0x100 << "\n";
  std::cout << 0xff << "\n";
  std::cout << 0xFF << "\n";
  int maxint = 0x7fffffff;
  std::cout << maxint << "\n";
  int x = 0xDEADBEEF;
  std::cout << x << "\n";
  x = 0xFFFFFFFF;
  std::cout << x << "\n";
  x = 0x80000000;
  std::cout << x << "\n";
}
