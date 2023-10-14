
// Represents a savings account
public class Savings extends Account{

    double interest; // The interest rate

    public Savings(int accountNum, int balance, String name, double interest){
        super(accountNum, balance, name);
        this.interest = interest;
    }
    
    public int withdraw(int amt) {
      if (amt > balance) {
        throw new RuntimeException(amt + " is not available");
      }
      else {
        this.balance = balance - amt;
      }
      return balance;
    }
    
    public <R> R accept(AccountVisitor<R> visitor) { return visitor.visitSavings(this); }
}
