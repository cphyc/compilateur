class A { public: };
class B : public A { public: };

int main() { B *x = new A(); }

