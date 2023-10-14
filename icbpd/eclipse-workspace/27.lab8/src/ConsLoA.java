
// Represents a non-empty List of Accounts...
public class ConsLoA extends AVertex{

    Account first;

    public ConsLoA(Account first, ALoA rest){
      super(rest);
      this.first = first;
      
    }
    
    /* Template
     *  Fields:
     *    ... this.first ...         --- Account
     *    ... this.rest ...          --- ILoA
     *
     *  Methods:
     *
     *  Methods for Fields:
     *
     */
    
    public Account findAccountByNum(int acctNum) {
      if (this.first.accountNum == acctNum) { return first; }
      else { return rest.findAccountByNum(acctNum); }
    }
    
    void removeAccountHelp(int acctNum, AVertex prev) {
      if (this.first.accountNum == acctNum) {
        prev.rest = this.rest;
      }
      else {
        this.rest.removeAccountHelp(acctNum, this);
      }
    }
}