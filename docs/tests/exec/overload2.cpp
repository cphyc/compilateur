#include <iostream>

class A {
public:
  int a;
  A();
  A(int a_param);
};

A::A() { a = 1; }
A::A(int a_param) { a = a_param; }

int main() {
  if ((new A())->a == 1 && (new A(2))->a == 2)
    std::cout << "ok\n";
}
