#include <iostream>

class A { public: };

int main() {
  int *p = NULL;
  std::cout << (p == NULL) << "\n";
  int x = 1;
  std::cout << (&x == NULL) << "\n";
  A a;
  std::cout << (&a == NULL) << "\n";
}
