#include <iostream>

int main() {
  int i = 0;
  int cpt = 0;
  for( ; i < 10; ) {
    int j;
    j = 10;
    for ( ; j > 0; ) {
      ++cpt;
      --j;
    }
    i++;
  }
  if (cpt == 100)
    std::cout << "ok\n";
}
