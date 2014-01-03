#include <iostream>

class interval {
public:
  int low, hi;
  interval(int low, int hi);
  void iterate(int& i);
  int ok(int& i);
  int next(int& i);
};

interval::interval(int low, int hi) {
  this->low = low;
  this->hi  = hi;
}

void interval::iterate(int& i) { i = low; }

int interval::ok(int& i) { return i <= hi; }

int interval::next(int& i) { return i++; }

int main() {
  interval s = interval(3, 10);
  int i;
  s.iterate(i);
  while(s.ok(i))
    std::cout << s.next(i) << "\n";
}
