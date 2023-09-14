import tester.*;

// Represents a mode of transportation
interface IMOT {

  // returns true if this mode of transportation is at least
  // as efficient as the given mpg, false otherwise
  boolean isMoreFuelEfficientThan(int mpg);
  
  boolean compareFuelEfficiency(IMOT other_mot);
  
}
 
// Represents a bicycle as a mode of transportation
class Bicycle implements IMOT {
  String brand;
 
  Bicycle(String brand) {
    this.brand = brand;
  }
  
// a bicycle does not consume fuel, so it will always be more fuel efficient
  public boolean isMoreFuelEfficientThan(int mpg) {
    return true;
  }
  
  public boolean compareFuelEfficiency(IMOT other_mot) {
    return true;
  }
  
}
 
// Represents a car as a mode of transportation
class Car implements IMOT {
  String make;
  int mpg; // represents the fuel efficiency in miles per gallon
 
  Car(String make, int mpg) {
    this.make = make;
    this.mpg = mpg;
  }
  
//compare this car's fuel efficiency to the given fuel efficiency
  public boolean isMoreFuelEfficientThan(int mpg) {
    return this.mpg >= mpg;
  }
  
  public boolean compareFuelEfficiency(IMOT other_mot) {
    return !other_mot.isMoreFuelEfficientThan(this.mpg);
  }
  
}
 
// Keeps track of how a person is transported
class TPerson {
  String name;
  IMOT mot;
 
  TPerson(String name, IMOT mot) {
    this.name = name;
    this.mot = mot;
  }
  
  boolean motMeetsFuelEfficiency(int mpg) {
    return this.mot.isMoreFuelEfficientThan(mpg);
  }
  
  boolean motIsMoreFuelEfficientThan(IMOT other_mot) {
    return this.mot.compareFuelEfficiency(other_mot);
  }
  
}

class ExamplesPerson {
  IMOT diamondback = new Bicycle("Diamondback");
  IMOT toyota = new Car("Toyota", 30);
  IMOT lamborghini = new Car("Lamborghini", 17);
 
  TPerson bob = new TPerson("Bob", diamondback);
  TPerson ben = new TPerson("Ben", toyota);
  TPerson becca = new TPerson("Becca", lamborghini);
  
  boolean testFuelEfficient (Tester t) {
    return
        t.checkExpect(this.ben.motIsMoreFuelEfficientThan(lamborghini), true);
  }
}