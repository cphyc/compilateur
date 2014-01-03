#include <iostream>

// liste simplement chaînée

class cell {
public:
  int v;
  cell *next;
  cell(int v, cell* next);
};

cell::cell(int v, cell* next) {
  this->v = v;
  this->next = next;
}

// liste chaînée de size éléments, avec accès à l'élément i (en temps O(i))

class list {
public:
  cell *head;
  list(int size);
  int& get(int i);
};

list::list(int size) {
  this->head = NULL;
  while (size-- > 0) this->head = new cell(0, this->head);
}

int& list::get(int i) {
  cell *c = this->head;
  while (i-- > 0) c = c->next;
  return c->v;
}

// on s'en sert comme un tableau pour calculer les premiers nombres
// de Fibonacci (en temps quadratique, donc)

int main() {
  list l = list(11);
  l.get(1) = 1;
  int i;
  for (i = 2; i < 11; i++) {
    l.get(i) = l.get(i-2) + l.get(i-1);
    std::cout << "F(" << i << ") = " << l.get(i) << "\n";
  }
}
