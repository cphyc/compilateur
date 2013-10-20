
// triangle de Pascal modulo 7
// on n'a pas de tableaux, alors on utilise des listes chaînées

#include <iostream>

class Array {
public:
  int val;
  Array *next;
  Array(int n);    // une liste à n éléments
  int get(int i);
  void set(int i, int v);
};

Array::Array(int n) {
  if (n == 1) return;
  next = new Array(n-1);
}

int Array::get(int i) {
  if (i == 0) return val;
  return next->get(i-1);
}

void Array::set(int i, int v) {
  if (i == 0) val = v;
  else next->set(i-1, v);
}

void print_row(Array *r, int i) {
  int j;
  for (j = 0; j <= i; j++) {
    if (r->get(j) != 0)
      std::cout << "*";
    else
      std::cout << "0";
  }
  std::cout << "\n";
}

void compute_row(Array *r, int j) {
  int v = 0;
  if (j == 0)
    v = 1;
  else
    v = (r->get(j) + r->get(j-1)) % 7;
  r->set(j, v);
  if (j > 0)
    compute_row(r, j-1);
}

int main() {
  int h = 42;
  Array *r = new Array(h+1);
  int i;
  for (i = 0; i < h; i++) {
    r->set(i, 0);
    compute_row(r, i);
    print_row(r, i);
  }
}
