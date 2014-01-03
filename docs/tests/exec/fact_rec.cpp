#include <iostream>

int fact_rec(int n) {
  if (n <= 1) return 1;
  return n * fact_rec(n - 1);
}

int main() {
  std::cout << fact_rec(5) << "\n";
}
