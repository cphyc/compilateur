#include <iostream>

class Vehicle {
public:
  int position;
  Vehicle();
  virtual void move(int d);
};

Vehicle::Vehicle() {
  position = 10;
}

void Vehicle::move(int d) {
  std::cout << "vehicle moves\n";
  position = position + d;
}


class Car : public Vehicle {
public:
  // champ position hérité
  int passengers;
  Car();
  // methode move() héritée
  void await(Vehicle &v);
};

Car::Car() {
}

void Car::await(Vehicle &v) {
  std::cout << "await: position = " << v.position << "\n";
  if (v.position < position)
    v.move(position - v.position);
  else
    move(10);
}

class Truck : public Vehicle {
public:
  // champ position hérité
  int load;
  Truck();
  void move(int d);
};

Truck::Truck() {
}

void Truck::move(int d) { // methode move redéfinie
  std::cout << "truck moves\n";
  if (d <= 55) position = position + d; else position = position + 55;
}

int main() {
  Truck t;
  std::cout << "t at " << t.position << "\n";
  Car c;
  c.passengers = 2;
  std::cout << "c at " << c.position << "\n";
  c.move(60);
  std::cout << "c at " << c.position << "\n";
  Vehicle *v = &c; // alias
  v->move(70);
  std::cout << "c at " << c.position << "\n";
  c.await(t);
  std::cout << "t at " << t.position << "\n";
  std::cout << "c at " << c.position << "\n";

}

/*
Local Variables:
compile-command: "make vehicles"
End:
*/

