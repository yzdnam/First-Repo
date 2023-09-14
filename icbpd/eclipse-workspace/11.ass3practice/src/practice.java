import tester.*;

interface ILin {
  int howMany(int i);
  int size();
  ILin rem(int i);
}

interface ICollection {
  
  boolean in(int i);
  int howMany(int i);
  int size();
  ICollection rem(int i);
  
}

class Cin implements ILin {
  int first;
  ILin rest;
  Cin(int first, ILin rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public int howMany(int i) {
    if (this.first == i) {
      return 1 + rest.howMany(i);
    }
    else { return rest.howMany(i); }
  }
  
  public int size() {
    return 1 + rest.size();
  }
  
  public ILin rem(int i) {
    if (this.first == i) {
      return this.rest;
    }
    else { return new Cin(this.first, this.rest.rem(i)); }
  }
}

class Mtin implements ILin {
  Mtin() {}
  
  public int howMany(int i) {
    return 0;
  }
  
  public int size() {
    return 0;
  }
  
  public ILin rem(int i) {
    return this;
  }
}

class Set extends Bag{
  Set(ILin elements) {
    super(elements);
  }
  
  // add i to this set
  // unless it is already there
  Set add(int i) {
    if (this.in(i)) {
      return this;
    }
    else {
      return
          new Set(new Cin(i,this.elements));
    }
  }
  
  public ICollection rem(int i) {
    return new Set(this.elements.rem(i));
  }
  
}

// a bag of integers
class Bag implements ICollection {
  ILin elements;
  Bag(ILin elements) {
    this.elements = elements;
  }
  
  // add i to this bag
  Bag add(int i) {
    return
        new Bag(new Cin(i,this.elements));
  }
  
  // is i a member of this bag?
  public boolean in(int i) {
    return this.elements.howMany(i) > 0;
  }
  
  // how often is i in this bag?
  public int howMany(int i) {
    return
        this.elements.howMany(i);
  }
  
  public int size() {
    return this.elements.size();
  }
  
  public ICollection rem(int i) {
    return new Bag(this.elements.rem(i));
  }
}

class ExampleBagsAndSets {
  
  Bag b1 = new Bag(new Cin(1, new Cin(1, new Cin(2, new Mtin()))));
  Set s1 = new Set(new Cin(1, new Cin(2, new Cin(3, new Mtin()))));
  
  boolean testAdd(Tester t) {
    return
        t.checkExpect(this.b1.rem(1), new Bag(new Cin(1, new Cin(2, new Mtin())))) &&
        t.checkExpect(this.s1.rem(1), new Set(new Cin(2, new Cin(3, new Mtin())))) &&
        t.checkExpect(this.b1.size(), 3) &&
        t.checkExpect(this.s1.size(), 3) &&
        t.checkExpect(this.b1.add(2), new Bag(new Cin(2,b1.elements))) &&
        t.checkExpect(this.s1.add(4), new Set(new Cin(4,s1.elements))) &&
        t.checkExpect(this.s1.add(1), this.s1) &&
        t.checkExpect(this.b1.in(3), false) &&
        t.checkExpect(this.b1.howMany(1), 2);
  }
}