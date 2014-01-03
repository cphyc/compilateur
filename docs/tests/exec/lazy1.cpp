#include <iostream>

int main() {
  std::cout << (42 || (1/0)) << "\n";
  std::cout << (0  && (2/0)) << "\n";
  int *p = NULL;
  if (1 || *p)
    std::cout << "ok\n";
}
