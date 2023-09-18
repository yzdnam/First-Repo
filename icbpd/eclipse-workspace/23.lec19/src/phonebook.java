import tester.*;

class ExamplePhoneLists {
 
  IList<Person> friends, family, work;
  Person anne, bob, clyde, dana, eric, frank, gail, henry, irene, jenny;
  void initData() {
    this.anne = new Person("Anne", 1234);
    this.bob = new Person("Bob", 3456);
    this.clyde = new Person("Clyde", 6789);
    this.dana = new Person("Dana", 1357);
    this.eric = new Person("Eric", 12469);
    this.frank = new Person("Frank", 7294);
    this.gail = new Person("Gail", 9345);
    this.henry = new Person("Henry", 8602);
    this.irene = new Person("Irene", 91302);
    this.jenny = new Person("Jenny", 8675309);
    this.friends =
      new ConsList<Person>(this.anne, new ConsList<Person>(this.clyde,
        new ConsList<Person>(this.gail, new ConsList<Person>(this.frank,
          new ConsList<Person>(this.jenny, new MtList<Person>())))));
    this.family =
      new ConsList<Person>(this.anne, new ConsList<Person>(this.dana,
        new ConsList<Person>(this.frank, new MtList<Person>())));
    this.work =
      new ConsList<Person>(this.bob, new ConsList<Person>(this.clyde,
        new ConsList<Person>(this.dana, new ConsList<Person>(this.eric,
          new ConsList<Person>(this.henry, new ConsList<Person>(this.irene,
            new MtList<Person>()))))));
  }
  
  //In ExamplePhoneLists
  void testFindPhoneNum(Tester t) {
    this.initData();
    t.checkExpect(this.friends.find(new findPhoneNum("Frank")), frank);
    t.checkExpect(this.family.find(new findPhoneNum("Frank")),
        this.friends.find(new findPhoneNum("Frank")));
    t.checkExpect(this.frank.phone, 7294);
    this.family.find(new findPhoneNum("Frank"), new changePhone(9021));
    t.checkExpect(this.friends.find(new findPhoneNum("Frank")), frank);
    t.checkExpect(this.family.find(new findPhoneNum("Frank")),
        this.friends.find(new findPhoneNum("Frank")));
    t.checkExpect(this.frank.phone, 9021);
  }
}

class Person {
  String name;
  int phone;
  Person(String name, int phone) {
    this.name = name;
    this.phone = phone;
  }
  // Returns true when the given person has the same name and phone number as this person
  boolean samePerson(Person that) {
    return this.name.equals(that.name) && this.phone == that.phone;
  }
}


class findPhoneNum implements IPred<Person> {
  String requestedName;
  findPhoneNum(String requestedName) {
    this.requestedName = requestedName;
  }
  public boolean apply(Person p) {
    return p.name.equals(requestedName);
  }
}

class changePhone implements IFunc<Person, Void> {
  int newNumber;
  changePhone(int newNumber) {
    this.newNumber = newNumber;
  }
  public Void apply(Person p) {
    p.phone = newNumber;
    return null;
  }
}