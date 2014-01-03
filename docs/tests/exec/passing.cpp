#include <iostream>

void incr(int &y) {
  y = y+1;
}

int main() {
  int x = 5;
  std::cout << "x = " << x << "\n";
  incr(x);
  std::cout << "x = " << x << "\n";
}

