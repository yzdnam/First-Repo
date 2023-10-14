
// Represents the empty List of Accounts
public class MtLoA extends ALoA {
    
    MtLoA(){}
    
    public Account findAccountByNum(int acctNum) {
      throw new RuntimeException("Account " + acctNum + " does not exist");
    }
    
    void removeAccountHelp(int acctNum, AVertex prev) { 
      throw new RuntimeException("Account " + acctNum + " does not exist");
    }
}

