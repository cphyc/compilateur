#include <iostream>

int main() {
  int i = 42;
  std::cout << i << "\n";
  if (1) {
    int j = 1;
    std::cout << j << "\n";
  } else {
    int j = 2;
    std::cout << j << "\n";
  }
  std::cout << i << "\n";
}
