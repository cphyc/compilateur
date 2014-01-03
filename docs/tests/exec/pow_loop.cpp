#include <iostream>

class Puiss {
public:
  int pow(int a, int n);
};

int Puiss::pow(int a, int n) {
  int r = 1;
  int i;
  for(i = 0; i < n / 2; i++)
    r = r * a;
  r = r * r;
  if (n % 2 != 0)
    r = r * a;
  return r;
}

int main() {
  Puiss p;
  std::cout << p.pow(2, 4) << "\n";
  std::cout << p.pow(6, 3) << "\n";
}
