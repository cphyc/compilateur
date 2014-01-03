#include <iostream>

class A {
public:
  A();
};

A::A() {
  std::cout << "brand new world\n";
}

int main() {
  A *a = new A();
}
