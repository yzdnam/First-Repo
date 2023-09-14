import tester.*;

interface ILoS {
  ILoS sort();
  ILoS insert(String other);
  boolean isSorted();
  boolean after(String other);
  ILoS interleave(ILoS other);
  ILoS merge(ILoS other);
  ILoS reverse();
  ILoS insertAtEnd(String other);
  boolean isDoubledList();
  boolean firstSame(String other);
  boolean isDoubledListHelp();
  boolean sameList(ILoS other);
  boolean sameListHelp(ILoS other);
  boolean isPalindromeList();
}

class ConsS implements ILoS {
  String first;
  ILoS rest;
  ConsS(String first, ILoS rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public ILoS sort() {
    return this.rest.sort().insert(this.first);
  }
  
  public ILoS insert(String other) {
    if (this.first.compareTo(other.toLowerCase()) > 0) {
      return new ConsS(other, this);
    }
    else { 
      return new ConsS(this.first, this.rest.insert(other)); 
    }
  }
  
  public boolean isSorted() {
    return this.rest.after(this.first) && this.rest.isSorted();
  }
  
  // true if the first string of this is alphabetically after the given string
  public boolean after(String other) {
    return this.first.toLowerCase().compareTo(other.toLowerCase()) >= 0;
  }
  
  public ILoS interleave(ILoS other) {
    return new ConsS(this.first, other.interleave(this.rest));
  }
  
  public ILoS merge(ILoS other) {
    if (other.after(this.first)) {
      return new ConsS(this.first, this.rest.merge(other));
    }
    else {
      return other.merge(this);
    }
  }
  
  public ILoS reverse() {
    return this.rest.reverse().insertAtEnd(this.first);
  }
  
  public ILoS insertAtEnd(String other) {
    return new ConsS(this.first, this.rest.insertAtEnd(other)); 
    }
  
  public boolean isDoubledList() {
    if (this.rest.firstSame(this.first)) {
      return this.rest.isDoubledListHelp();
    }
    else { return false; }
  }
  
  // checks if the first String of a list is the same as the given String
  public boolean firstSame(String other) {
    return this.first.compareTo(other) == 0;
  }
  
  // discards the first String in a list and checks the rest of the list for doubledness
  public boolean isDoubledListHelp() {
    return this.rest.isDoubledList();
  }
  
  public boolean sameList(ILoS other) {
    if (other.firstSame(this.first)) {
      return other.sameListHelp(this.rest);
    }
    else { return false; }
  }
  
  public boolean sameListHelp(ILoS other) {
    return this.rest.sameList(other);
  }
  
  public boolean isPalindromeList() {
    return this.sameList(this.reverse());
  }
  
}

class MtLoS implements ILoS {
  MtLoS() {}
  
  public ILoS sort() { return this; }
  
  public ILoS insert(String other) { return new ConsS(other, this); }
  
  public boolean isSorted() { return true; }
  
  public boolean after(String other) { return true; }
  
  public ILoS interleave(ILoS other) {
    return other;
  }
  
  public ILoS merge(ILoS other) {
    return other;
  }
  
  public ILoS reverse() {
    return this;
  }
  
  public ILoS insertAtEnd(String other) {
    return new ConsS(other, this);
  }
  
  public boolean isDoubledList() {
    return true;
  }
  
  public boolean firstSame(String other) {
    return false;
  }
  
  public boolean isDoubledListHelp() {
    return false;
  }
  
  public boolean sameList(ILoS other) {
    return other.sameListHelp(this);
  }
  
  public boolean sameListHelp(ILoS other) {
    return true;
  }
  
  public boolean isPalindromeList() {
    return true;
  }
}

class ExampleString {
  
  ConsS l1 = new ConsS("Zoom", new ConsS("bear", new ConsS("deer", new ConsS("april", new MtLoS()))));
  ConsS l2 = new ConsS("april", new ConsS("bear", new ConsS("deer", new ConsS("Zoom", new MtLoS()))));
  ConsS l3 = new ConsS("april", new ConsS("bear", new MtLoS()));
  ConsS l4 = new ConsS("abbacus", new ConsS("bass", new MtLoS()));
  ConsS l5 = new ConsS("arrange", new ConsS("blue", new MtLoS()));
  ConsS dl1 = new ConsS("a", new ConsS("a", new ConsS("b", new MtLoS())));
  ConsS dl2 = new ConsS("a", new ConsS("a", new MtLoS()));
  
  boolean testSort (Tester t) {
    return
        t.checkExpect(this.l1.sort(), this.l2) &&
        t.checkExpect(this.l1.isSorted(), false) &&
        t.checkExpect(this.l2.isSorted(), true) &&
        t.checkExpect(this.l1.interleave(this.l2), 
            new ConsS("Zoom", new ConsS("april", new ConsS("bear", new ConsS("bear", new ConsS("deer", new ConsS("deer", new ConsS("april", 
                new ConsS("Zoom", new MtLoS()))))))))) &&
        t.checkExpect(this.l3.interleave(this.l4), 
            new ConsS("april", new ConsS("abbacus", new ConsS("bear", new ConsS("bass", new MtLoS()))))) &&
        t.checkExpect(this.l3.merge(this.l4), 
            new ConsS("abbacus", new ConsS("april", new ConsS("bass", new ConsS("bear", new MtLoS()))))) &&
        t.checkExpect(this.l3.interleave(this.l5), 
            new ConsS("april", new ConsS("arrange", new ConsS("bear", new ConsS("blue", new MtLoS()))))) &&
        t.checkExpect(this.l3.merge(this.l5), 
            new ConsS("april", new ConsS("arrange", new ConsS("bear", new ConsS("blue", new MtLoS()))))) &&
        t.checkExpect(this.l4.reverse(), new ConsS("bass", new ConsS("abbacus", new MtLoS()))) &&
        t.checkExpect(this.dl1.isDoubledList(), false) &&
        t.checkExpect(this.dl2.isDoubledList(), true) &&
        t.checkExpect(this.l1.sameList(this.l1), true);
  }
}