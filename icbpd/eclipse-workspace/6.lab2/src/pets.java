import tester.*;

// to represent a pet owner
class Person {
  String name;
  IPet pet;
  int age;

  Person(String name, IPet pet, int age) {
    this.name = name;
    this.pet = pet;
    this.age = age;
  }

  boolean isOlder(Person other) {
    return other.isOlderHelper(this.age);
  }

  boolean isOlderHelper(int other_age) {
    return other_age > this.age;
  }

  // does the name of this person's pet match the given name?
  boolean samePetName(String other_name) {
    return this.pet.sameName(other_name);
  }
  
  Person perish() {
    return new Person(this.name, new NoPet(), this.age);
  }

}

// to represent a pet
interface IPet {
  boolean sameName(String other_name);
}

class NoPet implements IPet {
  NoPet() {
  }

  public boolean sameName(String other_name) {
    return false;
  }
  
}

// to represent a pet cat
  class Cat implements IPet {
    String name;
    String kind;
    boolean longhaired;

    Cat(String name, String kind, boolean longhaired) {
      this.name = name;
      this.kind = kind;
      this.longhaired = longhaired;
    }

    public boolean sameName(String other_name) {
      return this.name.compareTo(other_name) == 0;
    }
  }

// to represent a pet dog
  class Dog implements IPet {
    String name;
    String kind;
    boolean male;

    Dog(String name, String kind, boolean male) {
      this.name = name;
      this.kind = kind;
      this.male = male;
    }

    public boolean sameName(String other_name) {
      return this.name.compareTo(other_name) == 0;
    }
  }

class ExamplePetOwners {
  
  NoPet nopet = new NoPet();
  Cat c1 = new Cat("c1", "tabby", true);
  Cat c2 = new Cat("c2", "tiger", false);
  Dog d1 = new Dog("d1", "bull", true);
  Dog d2 = new Dog("d2", "terrier", false);
  
  Person p1 = new Person("p1", c1, 19);
  Person p2 = new Person("p2", c2, 20);
  Person p3 = new Person("p3", d1, 21);
  Person p4 = new Person("p4", d2, 22);
  Person p5 = new Person("p5", nopet, 23);
  
  boolean testSamePetName (Tester t) {
    return
        t.checkExpect(this.p1.samePetName("c1"), true) &&
        t.checkExpect(this.p1.samePetName("d2"), false);
  }
}