#include <iostream>

class A {
public:
  virtual void f();
};

class B : public A {
public:
  void f();
};

void A::f() {
  std::cout << "this is A::f" << "\n";
}

void B::f() {
  std::cout << "this is B::f" << "\n";
}

int main() {
  A a;
  a.f();
  B b;
  b.f();
  A *x = &b;
  x->f();
  x = &a;
  x->f();
}
