#include <iostream>

void plot(int y) {
  while (y-- > 0) std::cout << " ";
  std::cout << "X\n";
}

// suppose 0 <= y2 <= x2 (premier octant)
void bresenham(int x2, int y2) {
  int x = 0;
  int y = 0;
  int e = 2 * y2 - x2;
  for (x = 0; x <= x2; x++) {
    plot (y);
    if (e < 0)
      e = e + 2* y2;
    else {
      y++;
      e = e + 2 * (y2 - x2);
    }
  }
}

int main() {
  bresenham(10, 6);
}
