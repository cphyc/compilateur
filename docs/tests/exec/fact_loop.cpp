#include <iostream>

int fact_loop(int n) {
  int r = 1;
  while (n > 1) r = r * n--;
  return r;
}

int main() {
  std::cout << fact_loop(5) << "\n";
}
