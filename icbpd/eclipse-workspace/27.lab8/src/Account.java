
// Represents a bank account
public abstract class Account {

    int accountNum;  // Must be unique
    int balance;     // Must remain above zero (others Accts have more restrictions)
    String name;     // Name on the account

    public Account(int accountNum, int balance, String name){
        this.accountNum = accountNum;
        this.balance = balance;
        this.name = name;
    }
    
    abstract int withdraw(int amount);
    
    int deposit(int funds) {
      balance = balance + funds;
      return balance;
    }
    
    abstract <R> R accept(AccountVisitor<R> visitor); 
}
