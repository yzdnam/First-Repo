class Examples {
  
  Dog huffle = new Dog("Hufflepuff", "Wheaton Terrier", 2012, "TX", true);
  Dog pearl = new Dog("Pearl", "Labrador Retriever", 2016, "MA", false);
  
  IceCream order1 = 
      new Scooped
      (new Scooped
          (new Scooped
              (new Scooped
                  (new EmptyServing(false), 
                      "mint chip"),
                  "coffee"),
              "black raspberry"),
          "caramel swirl");
  
  IceCream order2 =
      new Scooped
      (new Scooped
          (new Scooped
              (new EmptyServing(true),
                  "chocolate"),
              "vanilla"),
          "strawberry");
  
  Housing hovel = new Hut(5, 1);
  Housing winterfell = new Castle("Winterfell", "Stark", 500, 6);
  Housing crossroads = new Inn("Inn At The Crossroads", 50, 20, 12);
  
  Transportation horse1 = new Horse(hovel, winterfell, "Mr. Ed", "yellow");
  Transportation carriage1 = new Carriage(winterfell, crossroads, 2);
  
}

// Problem 1
class Dog {
  String name;
  String breed;
  int yob;
  String residence;
  boolean hypoallergenic;
  Dog(String name, String breed, int yob, String residence, boolean hypoallergenic) {
    this.name = name;
    this.breed = breed;
    this.yob = yob;
    this.residence = residence;
    this.hypoallergenic = hypoallergenic;
  }
}

// Problem 2
interface IceCream {}

class EmptyServing implements IceCream {
  boolean cone;
  EmptyServing(boolean cone) {
    this.cone = cone;
  }
}

class Scooped implements IceCream {
  IceCream more;
  String flavor;
  Scooped(IceCream more, String flavor) {
    this.more = more;
    this.flavor = flavor;
  }
}

// Problem 3
interface Housing {}

class Hut implements Housing {
  int capacity;
  int population;
  Hut(int capacity, int population) {
    this.capacity = capacity;
    this.population = population;
  }
}

class Inn implements Housing {
  String name;
  int capacity;
  int population;
  int stalls;
  Inn(String name, int capacity, int population, int stalls) {
    this.name = name;
    this.capacity = capacity;
    this.population = population;
    this.stalls = stalls;
  }
}

class Castle implements Housing {
  String name;
  String owners;
  int population;
  int carriages;
  Castle(String name, String owners, int population, int carriages) {
    this.name = name;
    this.owners = owners;
    this.population = population;
    this.carriages = carriages;
  }
}

interface Transportation {}

class Horse implements Transportation {
  Housing from;
  Housing to;
  String name;
  String color;
  Horse(Housing from, Housing to, String name, String color) {
    this.from = from;
    this.to = to;
    this.name = name;
    this.color = color;
  }
}

class Carriage implements Transportation {
  Housing from;
  Housing to;
  int tonnage;
  Carriage(Housing from, Housing to, int tonnage) {
    this.from = from;
    this.to = to;
    this.tonnage = tonnage;
  }
}