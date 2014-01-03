#include <iostream>

int main() {
  int expr1 =- 7 + 6 * 5 - 4 / 3 - 2;     // 20
  std::cout << expr1 << "\n";
  int expr2 = (7 + 6) * (5 - 4) / 3 - 2; // 2
  std::cout << expr2 << "\n";
  int expr3 = 7 + 6 * (5 - 4 / (3 - 2));  // 13
  std::cout << expr3 << "\n";
  int sum = expr1 % 8 + expr2 + expr3;    // 19
  std::cout << sum << "\n";
}
