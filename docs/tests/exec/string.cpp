#include <iostream>

int main() {
  std::cout << "abc\n";
  std::cout << "a\nc\n";
  std::cout << "a\\c\n";
  std::cout << "a\tc\n";
  std::cout << "a\"c\n";
  std::cout << "\x41\x42\x43\n";
}
