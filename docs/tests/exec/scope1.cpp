#include <iostream>

class A {
public:
  int x;
  int f();
  int g(int x);
};

int A::f() { int x = 0; x++; return x; }

int A::g(int x) { x++; return x; }

int main() {
  A a;
  a.x = 0;
  std::cout << a.x << "\n";
  std::cout << a.f() << "\n";
  std::cout << a.x << "\n";
  std::cout << a.g(1) << "\n";
  std::cout << a.x << "\n";
}
