#include <iostream>

class Puiss {
public:
  int pow(int a, int n);
};

int Puiss::pow(int a, int n) {
  if (n <= 0)
    return 1;
  int r = pow(a, n / 2);
  r = r * r;
  if (n % 2 == 0)
    return r;
  return r * a;
}

int main() {
  Puiss p;
  std::cout << p.pow(2, 4) << "\n";
  std::cout << p.pow(6, 3) << "\n";
}
