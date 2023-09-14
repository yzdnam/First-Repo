// represents a person
class Person {
  String name;
  int age;
  String gender;
  Address address;
  
  Person(String name, int age, String gender, Address address){
    this.name = name;
    this.age = age;
    this.gender = gender;
    this.address = address;
    }
}

// represents an address
class Address {
  String city;
  String state;
  
  Address(String city, String state){
    this.city = city;
    this.state = state;
  }
}

class ExamplesPersonsStates {
  ExamplesPersonsStates(){}
  
  // examples of addresses
  Address boston = new Address("Boston", "Massachusetts");
  Address warwick = new Address("Warwick", "Rhode Island");
  Address nashua = new Address("Nashua", "New Hampshire");
  
  Person tim = new Person("Tim", 23, "Male", this.boston);
  Person kate = new Person("Kate", 22, "Female", this.warwick);
  Person rebecca = new Person("Rebecca", 31, "Non-binary", this.nashua);
  Person john = new Person("John", 32, "Male", this.boston);
  Person jamal = new Person("Jamal", 42, "Male", this.warwick);
}

// represents foods that offer a vegetarian option
interface MenuItem {
}

class Soup implements MenuItem {
  String name;
  int price; // in cents
  boolean vegetarian;
  
  Soup(String name, int price, boolean vegetarian) {
    this.name = name;
    this.price = price;
    this.vegetarian = vegetarian;
  }
}

class Salad implements MenuItem {
  String name;
  int price;
  boolean vegetarian;
  String dressing;
  
  Salad(String name, int price, boolean vegetarian, String dressing) {
    this.name = name;
    this.price = price;
    this.vegetarian = vegetarian;
    this.dressing = dressing;
  }
}

class Sandwich implements MenuItem {
  String name;
  int price;
  String bread;
  String filling1;
  String filling2;
  
  Sandwich(String name, int price, String bread, String filling1, String filling2) {
    this.name = name;
    this.price = price;
    this.bread = bread;
    this.filling1 = filling1;
    this.filling2 = filling2;
  }
}

class ExamplesMenuItems {
  ExamplesMenuItems() {}
  
  MenuItem caesar = new Salad("Caesar", 99, false, "Caesar");
  MenuItem italian = new Salad("Italian", 99, true, "Italian");    
}

/* An Ancestor Tree (AT) is one of
;; -- 'unknown
;; -- (make-tree Person AT AT)
 
;; A Person is defined as above */

interface AncestorTree {
}

class Unknown implements AncestorTree {
  
  Unknown() {}
  
}

class ATPerson implements AncestorTree {
  String name;
  AncestorTree left_branch;
  AncestorTree right_branch;
  
  ATPerson(String name, AncestorTree left_branch, AncestorTree right_branch) {
    this.name = name;
    this.left_branch = left_branch;
    this.right_branch = right_branch;
  }
}

class ExampleAncestorTree {
  ExampleAncestorTree(){}
  
  AncestorTree unknown = new Unknown();
  AncestorTree john = new ATPerson("John", this.unknown, this.unknown);
  
}