#include <iostream>

class A {
public:
    int a;
  A();
  void set_a();
  void set_a(int a);
};

A::A() { }
void A::set_a() { a = 1; }
void A::set_a(int a) { this->a = a; }

int main() {
  A a;
  int b;
  a.set_a();
  b = a.a == 1;
  a.set_a(2);
  b = b && a.a == 2;
  if (b)
    std::cout << "ok\n";
}

