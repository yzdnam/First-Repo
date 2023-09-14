import tester.*;

interface ILoI {
  
  boolean varASolution();
  boolean anyEvenV1();
  boolean anyPosOddV1();
  boolean anyBetween5and10V1();
  
  boolean varBSolution();
  ILoI anyEvenV2();
  ILoI anyPosOddV2();
  
  boolean varCSolution();
  ILoI anyBetween5and10V2();
  
    
}

class ConsLoI implements ILoI {
  int first;
  ILoI rest;
  ConsLoI(int first, ILoI rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public boolean varASolution() {
    return this.anyBetween5and10V1() && this.anyEvenV1() && this.anyPosOddV1();
  }
  
  public boolean anyEvenV1() {
    if ((this.first % 2) == 0) { return true; }
    else return this.rest.anyEvenV1(); 
  }
  
  public boolean anyPosOddV1() {
    if (!(this.first % 2 == 0) && this.first > 0) { return true; }
    else return this.rest.anyPosOddV1();
  }
  
  public boolean anyBetween5and10V1() {
    if (this.first >= 5 && this.first <= 10) { return true; }
    else return this.rest.anyBetween5and10V1();
  }
  
  public boolean varBSolution() {
    if (this.anyEvenV1()) {
      boolean test2 = this.anyEvenV2().anyPosOddV1();
      if (test2) {
        return this.anyEvenV2().anyPosOddV2().anyBetween5and10V1();
      }
      else { return false; }
    }
    else { return false; }
  }
  
  public ILoI anyEvenV2() {
    if ((this.first % 2) == 0) { return this.rest; }
    else return new ConsLoI(this.first, this.rest.anyEvenV2());
  }
  
  public ILoI anyPosOddV2() {
    if (!(this.first % 2 == 0) && this.first > 0) { return this.rest; }
    else return new ConsLoI(this.first, this.rest.anyPosOddV2());
  }
  
  public boolean varCSolution() {
    if (this.anyEvenV1()) {
      boolean test2 = this.anyEvenV2().anyPosOddV1();
      if (test2) {
        boolean test3 = this.anyEvenV2().anyPosOddV2().anyBetween5and10V1();
        if (test3) {
          return this.anyEvenV2().anyPosOddV2().anyBetween5and10V2().varCSolution();
        }
        else { return false; }
      }
      else { return false; }
    }
    else { return false; }
  }
  
  public ILoI anyBetween5and10V2() {
    if (this.first >= 5 && this.first <= 10) { return this.rest; }
    else return new ConsLoI(this.first, this.rest.anyBetween5and10V2());
  }
  
}

class MtLoI implements ILoI {
  MtLoI() {}
  
  public boolean varASolution() { return false; }
  
  public boolean anyEvenV1() { return false; }
  
  public boolean anyPosOddV1() { return false; }

  public boolean anyBetween5and10V1() { return false; }
  
  public boolean varBSolution() { return false; }
  
  public ILoI anyEvenV2() { return this; }
  
  public ILoI anyPosOddV2() { return this; }
  
  public boolean varCSolution() { return true; }
  
  public ILoI anyBetween5and10V2() { return this; }

}

class Prob2Examples {
  
  MtLoI end = new MtLoI();
  ConsLoI test1 = new ConsLoI(6, new ConsLoI(5, end));
  ConsLoI test2 = new ConsLoI(4, new ConsLoI(3, end));
  ConsLoI test3 = new ConsLoI(6, test1);
  ConsLoI test4 = new ConsLoI(42, test3);
  
  boolean testVarASol(Tester t) {
    return
        t.checkExpect(this.test1.varASolution(), true) &&
        t.checkExpect(this.test2.varASolution(), false) &&
        t.checkExpect(this.test1.varBSolution(), false) &&
        t.checkExpect(this.test3.varBSolution(), true) &&
        t.checkExpect(this.test4.varCSolution(), false) &&
        t.checkExpect(this.test3.varCSolution(), true);
  }
}