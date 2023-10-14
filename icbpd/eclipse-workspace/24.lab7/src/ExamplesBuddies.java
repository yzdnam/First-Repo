import tester.*;


// runs tests for the buddies problem
public class ExamplesBuddies{

  Person ann, bob, cole, dan, ed, fay, gabi, hank, jan, kim, len;
  
  void initData() {
  this.ann = new Person("Ann");
  this.bob = new Person("Bob");
  this.cole = new Person("Cole");
  this.dan = new Person("Dan");
  this.ed = new Person("Ed");
  this.fay = new Person("Fay");
  this.gabi = new Person("Gabi");
  this.hank = new Person("Hank");
  this.jan = new Person("Jan");
  this.kim = new Person("Kim");
  this.len = new Person("Len");
  
  this.ann.addBuddy(bob);
  this.ann.addBuddy(cole);
  
  this.bob.addBuddy(ann);
  this.bob.addBuddy(ed);
  this.bob.addBuddy(hank);
  
  this.cole.addBuddy(dan);
  
  this.dan.addBuddy(cole);
  
  this.ed.addBuddy(fay);
  
  this.fay.addBuddy(ed);
  this.fay.addBuddy(gabi);
  
  this.gabi.addBuddy(ed);
  this.gabi.addBuddy(fay);
  
  this.jan.addBuddy(kim);
  this.jan.addBuddy(len);
  
  this.kim.addBuddy(jan);
  this.kim.addBuddy(len);
  
  this.len.addBuddy(jan);
  this.len.addBuddy(kim);
  }
  
  void testHasDirectBuddy(Tester t) {
    this.initData();
    t.checkExpect(this.ann.hasDirectBuddy(cole), true);
    t.checkExpect(this.len.hasDirectBuddy(gabi), false);
  }
  
  void testCountCommonBuddies(Tester t) {
    this.initData();
    t.checkExpect(this.ann.countCommonBuddies(bob), 0);
    t.checkExpect(this.bob.countCommonBuddies(fay), 1);
  }
  
  void testExtendedBuddy(Tester t) {
    this.initData();
    t.checkExpect(this.ann.hasExtendedBuddy(gabi), true);
    t.checkExpect(this.ann.hasExtendedBuddy(jan), false);
  }
  void testAllBuddies(Tester t) {
    this.initData();
    t.checkExpect(this.jan.allBuddies(), new ConsLoBuddy(this.kim, new ConsLoBuddy(this.len, new ConsLoBuddy(this.jan, new MTLoBuddy()))));
    t.checkExpect(this.ann.allBuddies(),
        new ConsLoBuddy(this.gabi,
            new ConsLoBuddy(this.fay,
                new ConsLoBuddy(this.ed,
                    new ConsLoBuddy(this.hank,
                        new ConsLoBuddy(this.bob,
                            new ConsLoBuddy(this.dan,
                                new ConsLoBuddy(this.cole,
                                    new ConsLoBuddy(this.ann, new MTLoBuddy())))))))));
  }
  void testPartyCount(Tester t) {
    this.initData();
    t.checkExpect(this.ann.partyCount(), 7);
    t.checkExpect(this.jan.partyCount(), 2);
  }
}