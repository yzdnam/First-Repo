
// Represents a checking account
public class Checking extends Account{

    int minimum; // The minimum account balance allowed

    public Checking(int accountNum, int balance, String name, int minimum){
        super(accountNum, balance, name);
        this.minimum = minimum;
    }
    
    public int withdraw(int amt) {
      if (minimum > balance - amt) {
        throw new RuntimeException(amt + " is not available");
      }
      else {
        this.balance = balance - amt;
      }
      return balance;
    }
    
    public <R> R accept(AccountVisitor<R> visitor) { return visitor.visitChecking(this); }
}
