#include <iostream>

class fib {
public:
  int prev, cur;
  fib();
  int next();
};

fib::fib() {
  prev = 1; cur = 0;
}

int fib::next() { int i = cur; cur = prev + cur; prev = i; return i; }

int main() {
  fib s;
  int f;
  while((f = s.next()) <= 100)
    std::cout << f << "\n";
}
