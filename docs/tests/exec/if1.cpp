#include <iostream>

int main() {
  if (1)
    if (0)
      ;
    else {
      if (1 == 2)
        std::cout << (0/0);
      else
        std::cout << "hello world\n";
    }
}
