
// Represents a Bank with list of accounts
public class Bank {
    
    String name;
    MutableLoA accounts;
    
    public Bank(String name){
        this.name = name;

        // Each bank starts with no accounts
        this.accounts = new MutableLoA();
    }
    
    void add(Account acct) {
      this.accounts.add(acct);
    }
    
    Account findAccountByNum(int acctNum) {
      return this.accounts.findAccountByNum(acctNum);
    }
    
    int deposit(int acctNum, int funds) {
      return new AccountDeposit(funds).apply(this.findAccountByNum(acctNum));
    }
    
    int withdraw(int acctNum, int funds) {
      return new AccountWithdraw(funds).apply(this.findAccountByNum(acctNum));
    }
    
    void remove(int acctNum) {
      this.accounts.remove(acctNum);
    }

}

interface IFunc<T, U> {
  public U apply(T t);
}

interface AccountVisitor<R> extends IFunc<Account, R> {
  R visitCredit(Credit cr);
  R visitChecking(Checking ch);
  R visitSavings(Savings s);
}

class AccountDeposit implements AccountVisitor<Integer> {
  int funds;
  AccountDeposit(int funds) {
    this.funds = funds;
  }

  public Integer visitCredit(Credit cr) { return cr.deposit(funds); }
  public Integer visitChecking(Checking ch) { return ch.deposit(funds); }
  public Integer visitSavings(Savings s) { return s.deposit(funds); }
  
  public Integer apply(Account a) { return a.accept(this); }
}

class AccountWithdraw implements AccountVisitor<Integer> {
  int funds;
  AccountWithdraw(int funds) {
    this.funds = funds;
  }

  public Integer visitCredit(Credit cr) { return cr.withdraw(funds); }
  public Integer visitChecking(Checking ch) { return ch.withdraw(funds); }
  public Integer visitSavings(Savings s) { return s.withdraw(funds); }
  
  public Integer apply(Account a) { return a.accept(this); }
}