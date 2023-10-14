
// Represents a credit line account
public class Credit extends Account{

    int creditLine;  // Maximum amount accessible
    double interest; // The interest rate charged
    
    public Credit(int accountNum, int balance, String name, int creditLine, double interest){
        super(accountNum, balance, name);
        this.creditLine = creditLine;
        this.interest = interest;
    }
    
    public int withdraw(int amt) {
      if (amt + balance > creditLine) {
        throw new RuntimeException(amt + " is not available");
      }
      else {
        this.balance = balance + amt;
      }
      return balance;
    }
    
    int deposit(int funds) {
      if (funds > balance) {
        throw new RuntimeException(balance + " is the maximum amount that can be funded to this account");
      }
      else {
        balance = balance - funds;
      }
      return balance;
    }
    
    public <R> R accept(AccountVisitor<R> visitor) { return visitor.visitCredit(this); }
}
