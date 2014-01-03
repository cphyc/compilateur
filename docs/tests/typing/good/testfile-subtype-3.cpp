
class A { public: };
class B : public A { public: };
int main() { A *x = new B(); }

