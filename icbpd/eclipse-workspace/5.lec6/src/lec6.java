import tester.*;

interface IAT {
  
//To compute the number of known ancestors of this ancestor tree
//(excluding this ancestor tree itself)
  int count();
  
  int countHelp();

//To compute how many ancestors of this ancestor tree (excluding this
//ancestor tree itself) are women older than 40 (in the current year)?
  int femaleAncOver40();
  
  int femaleAncOver40Help();
  
  int numTotalGens();
  
  int numPartialGens();
  
//To determine if this ancestry tree is born in or before the given year
  boolean bornInOrBefore(int yob);
  
//To determine if this ancestry tree is older than the given year of birth,
//and its parents are well-formed
  boolean wellFormedHelp(int childYob);

//To compute whether this ancestor tree is well-formed: are all known
//people younger than their parents?
  boolean wellFormed();

//To compute this ancestor tree's youngest grandparent
  IAT youngestGrandparent();
  
//To return the younger of this ancestor tree and the given ancestor tree
  IAT youngerIAT(IAT other);
  
//To return either this Unknown (if this Unknown is younger than the
//given yob) or the given ancestry tree
  IAT youngerIATHelp(IAT other, int otherYob);
  
  IAT youngestParent();
  
//To compute the youngest ancestor in the given generation of this ancestry tree
  IAT youngestAncInGen(int gen);
  
  //compute sameness between this IAT and that IAT
  boolean sameIAT(IAT that);
  
  boolean sameUnknown(Unknown that);
  
  boolean samePerson(Person that);
}
class Unknown implements IAT {
   Unknown() { }
   
   // To compute the number of known ancestors of this Unknown (excluding this Unknown itself)
   public int count() { return 0; }
   
   public int countHelp() {return 0;}
   
   public int femaleAncOver40() {return 0;}
   
   public int femaleAncOver40Help() {return 0;}
   
   public int numTotalGens() {return 0;}
   
   public int numPartialGens() {return 0;}
   
   // To determine if this Unknown is born in or before the given year
   public boolean bornInOrBefore(int yob) { return true; }
   
   public boolean wellFormedHelp(int childYob) { return true; }
   
   public boolean wellFormed() {
     return true;
   }
   
   public IAT youngestGrandparent() {
     return new Unknown();
   }
   
// To return the younger of this Unknown and the given ancestor tree
   public IAT youngerIAT(IAT other) { return other; }
   
// To return either this Unknown (if this Unknown is younger than the
// given yob) or the given ancestry tree
   public IAT youngerIATHelp(IAT other, int otherYob) { return other; }
   
   public IAT youngestParent() { return new Unknown(); }
   
   public IAT youngestAncInGen(int gen) {
     if (gen == 0) {
         return this;
     }
     else {
         return new Unknown();
     }
   }
   
   public boolean sameIAT(IAT that) {
     return that.sameUnknown(this);
   }
   
   public boolean sameUnknown(Unknown that) {
     return true;
   }
   
   public boolean samePerson(Person that) {
     return false;
   }

}
class Person implements IAT {
    String name;
    int yob;
    boolean isMale;
    IAT mom;
    IAT dad;
    Person(String name, int yob, boolean isMale, IAT mom, IAT dad) {
        this.name = name;
        this.yob = yob;
        this.isMale = isMale;
        this.mom = mom;
        this.dad = dad;
    }
    
 // To compute the number of known ancestors of this Person (excluding this Person itself)
    public int count() {
        /* Template:
         * Fields:
         * this.mom -- IAT
         * this.dad -- IAT
         * Methods:
         * this.count() -- int
         * Methods of fields:
         * this.mom.count() -- int
         * this.dad.count() -- int
         */
      return this.mom.countHelp() + this.dad.countHelp();
    }
    
    public int countHelp() {
      return 1 + this.mom.countHelp() + this.dad.countHelp();
    }
    
    public int femaleAncOver40() {
      return this.mom.femaleAncOver40Help() + this.dad.femaleAncOver40Help();
    }
    
    public int femaleAncOver40Help() {
      if (2023 - this.yob > 40 && !this.isMale) {
        return 1 + this.mom.femaleAncOver40Help() + this.dad.femaleAncOver40Help();
      }
      else {
        return this.mom.femaleAncOver40Help() + this.dad.femaleAncOver40Help();
      }
    }
    
    public int numTotalGens() {
      return 1 + Math.min(this.mom.numTotalGens(), this.dad.numTotalGens());
    } 
    
    public int numPartialGens() {
      return 1 + Math.max(this.mom.numPartialGens(), this.dad.numPartialGens());
    }
    
    public boolean bornInOrBefore(int yob) {
      return this.yob <= yob;
    }
    
    public boolean wellFormedHelp(int childYob) {
      return this.yob <= childYob &&
             this.mom.wellFormedHelp(this.yob) &&
             this.dad.wellFormedHelp(this.yob);
    }
    
    public boolean wellFormed() {
      return this.mom.wellFormedHelp(this.yob) &&
          this.dad.wellFormedHelp(this.yob);  
    }
    
    public IAT youngestGrandparent() {
      return this.mom.youngestParent().youngerIAT(this.dad.youngestParent());
    }
    
    public IAT youngerIAT(IAT other) {
      return other.youngerIATHelp(this, this.yob);
    }
    
    public IAT youngerIATHelp(IAT other, int otherYob) {
      if (this.yob > otherYob) {
        return this;
      }
      else {
        return other;
      }
    }
    
    public IAT youngestParent() {
      return this.mom.youngerIAT(this.dad);
    }
    
    public IAT youngestAncInGen(int gen) {
      if (gen == 0) {
        return this;
      }
      else {
        return this.mom.youngestAncInGen(gen - 1).youngerIAT(this.dad.youngestAncInGen(gen - 1));
      }
    }
    
    public boolean sameIAT(IAT that) {
      return that.samePerson(this);
    }
    
    public boolean sameUnknown(Unknown that) {
      return false;
    }
    
    public boolean samePerson(Person that) {
      return this.name.equals(that.name) && 
          this.yob == that.yob &&
          this.isMale == that.isMale &&
          this.dad.sameIAT(that.dad) &&
          this.mom.sameIAT(that.mom);
    }
}

interface ILoString {
  
  ILoString dappend(ILoString other);
  ILoString dappendHelp(String otherFirst, ILoString otherRest);
  
  ILoString accumappend(ILoString other);
 // ILoString accumappendHelp(ILoString otherRest);
  
  int len();
  
}
class ConsLoString implements ILoString {
    String first;
    ILoString rest;
    ConsLoString(String first, ILoString rest) {
        this.first = first;
        this.rest = rest;
    }
    
    public ILoString dappend(ILoString other) {
      return other.dappendHelp(this.first, this.rest);
    }
    
    public ILoString dappendHelp(String otherFirst, ILoString otherRest) {
      return new ConsLoString(otherFirst, otherRest.dappend(this));
    }
    
    public int len() {
      return 1 + this.rest.len();
    }
    
    public ILoString accumappend(ILoString other) {
      return new ConsLoString (this.first, this.rest.accumappend(other));
    }
    
   /* public ILoString accumappendHelp(ILoString other) {
      return other.accumappend(this);
    }*/
}
class MtLoString implements ILoString {
    MtLoString() { }
    
    public ILoString dappend(ILoString other) {
      return other;
    }
    
    public ILoString dappendHelp(String otherFirst, ILoString otherRest) {
      return new ConsLoString(otherFirst, otherRest);
    }
    
    public int len() {
      return 0;
    }
    
    public ILoString accumappend(ILoString other) {
      return other;
    }
    
/*    public ILoString accumappendHelp(ILoString other) {
      return other;
    }*/
}

class ExamplesIAT {
  IAT enid = new Person("Enid", 1904, false, new Unknown(), new Unknown());
  IAT edward = new Person("Edward", 1902, true, new Unknown(), new Unknown());
  IAT emma = new Person("Emma", 1906, false, new Unknown(), new Unknown());
  IAT eustace = new Person("Eustace", 1907, true, new Unknown(), new Unknown());

  IAT david = new Person("David", 1925, true, new Unknown(), this.edward);
  IAT daisy = new Person("Daisy", 1927, false, new Unknown(), new Unknown());
  IAT dana = new Person("Dana", 1933, false, new Unknown(), new Unknown());
  IAT darcy = new Person("Darcy", 1930, false, this.emma, this.eustace);
  IAT darren = new Person("Darren", 1935, true, this.enid, new Unknown());
  IAT dixon = new Person("Dixon", 1936, true, new Unknown(), new Unknown());

  IAT clyde = new Person("Clyde", 1955, true, this.daisy, this.david);
  IAT candace = new Person("Candace", 1960, false, this.dana, this.darren);
  IAT cameron = new Person("Cameron", 1959, true, new Unknown(), this.dixon);
  IAT claire = new Person("Claire", 1956, false, this.darcy, new Unknown());

  IAT bill = new Person("Bill", 1980, true, this.candace, this.clyde);
  IAT bree = new Person("Bree", 1981, false, this.claire, this.cameron);

  IAT andrew = new Person("Andrew", 2001, true, this.bree, this.bill);
  
  ILoString tnull = new MtLoString();
  ILoString test1 = new ConsLoString("a", new ConsLoString("b", tnull));
  ILoString test2 = new ConsLoString("c", new ConsLoString("d", tnull));
  ILoString res1 = new ConsLoString("a", 
                       new ConsLoString("b", 
                           new ConsLoString("c",
                               new ConsLoString("d", tnull))));

  boolean testAppend(Tester t) {
    return
        t.checkExpect(test1.dappend(test2), res1) &&
        t.checkExpect(test1.accumappend(test2), res1);
  }
  boolean testCount(Tester t) {
      return
          t.checkExpect(this.andrew.count(), 16) &&
          t.checkExpect(this.david.count(), 1) &&
          t.checkExpect(this.enid.count(), 0) &&
          t.checkExpect(new Unknown().count(), 0);
  }
  boolean testFemaleAncOver40(Tester t) {
      return
          t.checkExpect(this.andrew.femaleAncOver40(), 8) &&
          t.checkExpect(this.bree.femaleAncOver40(), 3) &&
          t.checkExpect(this.darcy.femaleAncOver40(), 1) &&
          t.checkExpect(this.enid.femaleAncOver40(), 0) &&
          t.checkExpect(new Unknown().femaleAncOver40(), 0);
  }
  boolean testWellFormed(Tester t) {
      return
          t.checkExpect(this.andrew.wellFormed(), true) &&
          t.checkExpect(new Unknown().wellFormed(), true) &&
          t.checkExpect(
              new Person("Zane", 2000, true, this.andrew, this.bree).wellFormed(),
              false);
  }

  boolean testYoungestGrandparent(Tester t) {
      return
          t.checkExpect(this.emma.youngestGrandparent(), new Unknown()) &&
          t.checkExpect(this.david.youngestGrandparent(), new Unknown()) &&
          t.checkExpect(this.claire.youngestGrandparent(), this.eustace) &&
          t.checkExpect(this.bree.youngestGrandparent(), this.dixon) &&
          t.checkExpect(this.andrew.youngestGrandparent(), this.candace) &&
          t.checkExpect(new Unknown().youngestGrandparent(), new Unknown());
  }
  boolean testNumGens(Tester t) {
    return
        t.checkExpect(this.andrew.numTotalGens(), 3) &&
        t.checkExpect(this.andrew.numPartialGens(), 5) &&
        t.checkExpect(this.enid.numTotalGens(), 1) &&
        t.checkExpect(this.enid.numPartialGens(), 1) &&
        t.checkExpect(new Unknown().numTotalGens(), 0) &&
        t.checkExpect(new Unknown().numPartialGens(), 0);
  }
  
  boolean testSameIAT(Tester t) {
    return
        t.checkExpect(this.andrew.sameIAT(this.andrew), true) &&
        t.checkExpect(this.andrew.sameIAT(this.bill), false);
  }
}

