#include <iostream>

int main() {
  int i;
  int j;
  for (i = 0; i < 5; i++)
    for (j = 0; j < 5; j++)
      std::cout << "i*j = " << i*j << "\n";
  std::cout << "i = " << i << "\n";
  std::cout << "j = " << j << "\n";
}
