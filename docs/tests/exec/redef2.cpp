#include <iostream>

class A {
public:
  virtual int f(int x);
};

int A::f(int x) { return x; }

class B : public A {
public:
};

class C : public B {
public:
  virtual int f(int x);
};

int C::f(int x) { return 3*x; }

int main() {
  A *a = new A();
  std::cout << a->f(1) << "\n";
  B *b = new B();
  std::cout << b->f(1) << "\n";
  C *c = new C();
  std::cout << c->f(1) << "\n";
  a = b;
  std::cout << a->f(1) << "\n";
  a = c;
  std::cout << a->f(1) << "\n";
  b = c;
  std::cout << b->f(1) << "\n";
}
