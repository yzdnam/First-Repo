class MutableLoA {
  Sentinel sentinel;
  MutableLoA() {
    this.sentinel = new Sentinel(new MtLoA());
  }
  
  public void add(Account acct) {
    this.sentinel.add(acct);
  }
  
  public Account findAccountByNum(int acctNum) {
    return this.sentinel.findAccountByNum(acctNum);
  }
  
  public void remove(int acctNum) {
    this.sentinel.rest.removeAccountHelp(acctNum, this.sentinel);
  }
  
}

// Represents a List of Accounts
abstract class ALoA{

  abstract Account findAccountByNum(int acctNum);
  abstract void removeAccountHelp(int acctNum, AVertex prev);

}

abstract class AVertex extends ALoA {
  ALoA rest;
  AVertex(ALoA rest) {
    this.rest = rest;
  }

  abstract Account findAccountByNum(int acctNum);
}

class Sentinel extends AVertex {
  Sentinel(ALoA rest) {
    super(rest);
  }
  
  public void add(Account acct) { 
    rest = new ConsLoA(acct, rest); 
    }
  
  public Account findAccountByNum(int acctNum) { return rest.findAccountByNum(acctNum); }
  
  void removeAccountHelp(int acctNum, AVertex prev) {
    throw new RuntimeException("Can't try to remove on a Sentinel!");
  }
}

