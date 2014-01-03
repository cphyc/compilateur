
#include <iostream>

// arbres binaires, où l'arbre vide est NULL

class Node {
public:
  int elt;
  Node *left, *right;
  Node(Node *left, int elt, Node *right);
};

Node::Node(Node *left, int elt, Node *right) {
  this->left = left;
  this->elt = elt;
  this->right = right;
}

int tree_size(Node *t) {
  if (t == NULL) return 0;
  return 1 + tree_size(t->left) + tree_size(t->right);
}

Node *tree_add(Node *t, int x) {
  if (t == NULL) return new Node(NULL, x, NULL);
  if      (x < t->elt) t->left  = tree_add(t->left,  x);
  else if (x > t->elt) t->right = tree_add(t->right, x);
  return t;
}

void tree_add_ref(Node* &t, int x) {
  if      (t == NULL ) t = new Node(NULL, x, NULL);
  else if (x < t->elt) tree_add_ref(t->left,  x);
  else if (x > t->elt) tree_add_ref(t->right, x);
}

void tree_print(Node *t) {
  if (t == NULL) return;
  std::cout << "(";
  tree_print(t->left);
  std::cout << t->elt;
  tree_print(t->right);
  std::cout << ")";
}


// encapsulation dans une classe

class BST {
public:
  Node *root;
  BST();
  int size();
  void add1(int x);
  void add2(int x);
  void print();
};

BST::BST() {
  this->root = NULL;
}

int BST::size() {
  return tree_size(this->root);
}

void BST::add1(int x) {
  this->root = tree_add(this->root, x);
}

void BST::add2(int x) {
  tree_add_ref(this->root, x);
}

void BST::print() {
  tree_print(this->root);
  std::cout << "\n";
}


// tests

int main() {
  BST t;
  t.add1(2);
  t.add2(3);
  t.add1(1);
  t.print();
  t.add2(7);
  t.add1(0);
  t.print();
  BST *u = new BST();
  int i;
  for (i = 0; i < 10; i++)
    u->add1((31 * i) % 7);
  u->print();
  for (i = 0; i < 10; i++)
    u->add2((29 * i) % 13);
  u->print();
  return 0;
}
