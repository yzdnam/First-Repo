import tester.*;

interface IRiver {
  
  // count the number of sources for this river system
  int sources();
  
  boolean onRiver(Location aloc, double radius);
  
  int length();
  
  int maxLength();
  
  int confluences();
  
  ILoLocs locations();
  
}

interface ILoLocs {
	ILoLocs append(ILoLocs other);
	ILoLocs appendHelp(Location first, ILoLocs rest);
}

// a location on a river
class Location {
  int x;
  int y;
  String name;
  Location(int x, int y, String name) {
    this.x = x;
    this.y = y;
    this.name = name;
  }
  
  boolean withinRadius(Location aloc, double radius) {
    return radius > Math.sqrt(Math.pow((this.x - aloc.x),2) + Math.pow((this.y - aloc.y),2)) ;
  }
}

class MtLoLocs implements ILoLocs {
  MtLoLocs() {};
  
  public ILoLocs append(ILoLocs other) { return other; }
  public ILoLocs appendHelp(Location first, ILoLocs rest) { return new ConsLoLocs(first, rest); }
}

class ConsLoLocs implements ILoLocs {
  Location fst;
  ILoLocs rst;
  ConsLoLocs(Location fst, ILoLocs rst) {
    this.fst = fst;
    this.rst = rst;
  }
  
  public ILoLocs append(ILoLocs other) {
	  return other.appendHelp(this.fst, this.rst);
  }
  public ILoLocs appendHelp(Location first, ILoLocs rest) {
	  return new ConsLoLocs(first, rest.append(this));
  }
}

// the end of a river
class Mouth {
  Location loc;
  IRiver river;
  Mouth(Location loc, IRiver river) {
    this.loc = loc;
    this.river = river;
  }
  
  // count the number of sources that feed this Mouth
  int sources() {
    return this.river.sources();
  }
  
  boolean onRiver(Location aloc, double radius) {
    return this.loc.withinRadius(aloc, radius) ||
        this.river.onRiver(aloc, radius);
  }
  
  int length() {
    return this.river.length();
  }
  
  int maxLength() {
    return this.river.maxLength();
  }
  
  int confluences() {
    return this.river.confluences();
  }
  
  ILoLocs locations() {
    return new ConsLoLocs(this.loc, this.river.locations());
  }
}

// the source of a river
class Source implements IRiver {
  int miles;
  Location loc;
  Source(int miles, Location loc) {
    this.miles = miles;
    this.loc = loc;
  }
  
  public int sources() {
    return 1;
  }
  
  public boolean onRiver(Location aloc, double radius) {
    return this.loc.withinRadius(aloc, radius);
  }
  
  public int length() {
    return this.miles;
  }
  
  public int maxLength() {
    return this.miles;
  }
  
  public int confluences() {
    return 0;
  }
  
  public ILoLocs locations() {
    return new ConsLoLocs(this.loc, new MtLoLocs());
  }
}

// a confluence of two rivers
class Confluence implements IRiver {
  int miles;
  Location loc;
  IRiver left;
  IRiver right;
  Confluence(int miles, Location loc, IRiver left, IRiver right) {
    this.miles = miles;
    this.loc = loc;
    this.left = left;
    this.right = right;
  }
  
  public int sources() {
    return this.left.sources() + this.right.sources();
  }
  
  public boolean onRiver(Location aloc, double radius) {
    return this.loc.withinRadius(aloc, radius) ||
        this.left.onRiver(aloc, radius) ||
        this.right.onRiver(aloc, radius);
  }
  
  public int length() {
    return this.miles +
        this.left.length() +
        this.right.length();
  }
  
  public int maxLength() {
    if (this.right.maxLength() > this.left.maxLength()) {
      return this.miles + this.right.maxLength();
    }
    else {return this.miles + this.left.maxLength();}
  }
  
  public int confluences() {
    return 1 + this.left.confluences() + this.right.confluences();
  }
  
  public ILoLocs locations() {
	  return new ConsLoLocs(this.loc, this.left.locations().append(this.right.locations()));
  }
}


class RiverExamples {
  
  Location lm = new Location(7,5,"m");
  Location la = new Location (5,5,"a");
  Location lb = new Location(3,3,"b");
  Location ls = new Location(1,1,"s");
  Location lt = new Location(1,5,"t");
  Location lu = new Location(3,7,"u");
  
  IRiver s = new Source(3, ls);
  IRiver t = new Source(2, lt);
  IRiver u = new Source(1, lu);
  
  IRiver cb = new Confluence(3,lb,s,t);
  IRiver ca = new Confluence(4,la,cb,u);
  
  Mouth mth = new Mouth(lm,ca);
  
  MtLoLocs mtlolocs = new MtLoLocs();
  
  boolean testSources(Tester t) {
    return
        t.checkExpect(s.sources(), 1) &&
        t.checkExpect(ca.sources(), 3) &&
        t.checkExpect(cb.sources(), 2) &&
        t.checkExpect(mth.sources(), 3);
  }
  
  boolean testOnRiver(Tester t ) {
    return
        t.checkExpect(this.mth.onRiver(new Location(7,5,"testLoc"), 1.0), true) &&
        t.checkExpect(this.mth.onRiver(new Location(2,5,"testLoc"), 1.5), true);
  }
  
  boolean testLength(Tester t) {
    return
        t.checkExpect(this.s.length(), 3) &&
        t.checkExpect(this.t.length(), 2) &&
        t.checkExpect(this.u.length(), 1) &&
        t.checkExpect(this.cb.length(), 8) &&
        t.checkExpect(this.ca.length(), 13) &&
        t.checkExpect(this.mth.length(), 13);
  }
  
  boolean testMaxLength(Tester t) {
    return
        t.checkExpect(this.mth.maxLength(), 10);
  }
  
  boolean testConfluences(Tester t) {
    return
        t.checkExpect(this.mth.confluences(),2);
  }
  
  boolean testLocations(Tester t) {
    return
        t.checkExpect(this.mth.locations(), new ConsLoLocs(lm, 
                                                new ConsLoLocs(la, 
                                                    new ConsLoLocs(lb, 
                                                        new ConsLoLocs(ls, 
                                                            new ConsLoLocs(lt, 
                                                                new ConsLoLocs(lu, mtlolocs)))))));
  }
}
