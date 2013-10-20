#include <iostream>

int main() {
  std::cout << 0100 << "\n";
  std::cout << 0777 << "\n";
  std::cout << 0644 << "\n";
  int maxint = 017777777777;
  std::cout << maxint << "\n";
  int x = 037777777777;
  std::cout << x << "\n";
  x = 020000000000;
  std::cout << x << "\n";
}
