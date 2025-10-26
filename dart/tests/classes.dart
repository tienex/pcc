// Object-oriented programming in Dart
class Animal {
  String name;

  Animal(this.name);

  void speak() {
    print('Animal speaks');
  }
}

class Dog extends Animal {
  Dog(String name) : super(name);

  void speak() {
    print('$name says: Woof!');
  }
}

void main() {
  var dog = Dog('Rex');
  dog.speak();
}
