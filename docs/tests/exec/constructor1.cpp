#include <iostream>

class A {
public:
  int x;
  int y;
  A (int x, int y);
};

A::A(int x, int y) { this->x = x; this->y =y; }

int main() {
  A a = A(17, 42);
  std::cout << a.x << "\n";
  std::cout << a.y << "\n";
}
