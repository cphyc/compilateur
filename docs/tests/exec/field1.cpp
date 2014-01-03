#include <iostream>

class A {
public:
  int x;
};

class B : public A {
public:
  int x;
};

int main() {
  A *a = new A();
  a->x = 0;
  std::cout << a->x << "\n";
  B *b = new B();
  b->x = 1;
  std::cout << b->x << "\n";
  a = b;
  std::cout << a->x << "\n";
}
