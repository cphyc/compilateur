#include <iostream>

class A {
public:
  virtual int f(int x);
};

int A::f(int x) { return x; }

class B : public A {
public:
  virtual int f(int x);
};

int B::f(int x) { return 2*x; }

int main() {
  A *a = new A();
  std::cout << a->f(1) << "\n";
  B *b = new B();
  std::cout << b->f(1) << "\n";
  a = b;
  std::cout << a->f(1) << "\n"; // c'est B::f qui est appelée
}
