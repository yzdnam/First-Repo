import tester.*;

// Bank Account Examples and Tests
public class Examples {

    public Examples(){ reset(); }
    
    Account check1;
    Account savings1;
    Account check2;
    Account savings2;
    Account credit1;
    
    Bank testBank;
    
    // Initializes accounts to use for testing with effects.
    // We place inside reset() so we can "reuse" the accounts
    public void reset(){
        
        // Initialize the account examples
        check1 = new Checking(1, 100, "First Checking Account", 20);
        savings1 = new Savings(4, 200, "First Savings Account", 2.5);
        check2 = new Checking(2, 150, "Second Checking Account", 10);
        savings2 = new Savings(5, 250, "Second Savings Account", 2.0);
        credit1 = new Credit(10, 300, "First Credit Account", 1000, 4.5);
        
        // Initialize the bank example
        testBank = new Bank("TestBank");
    }
    
    public void setBank() {
      this.reset();
      this.testBank.add(check1);
      this.testBank.add(check2);
      this.testBank.add(savings1);
      this.testBank.add(savings2);
      this.testBank.add(credit1);
    }
    
    // Tests the exceptions we expect to be thrown when
    //   performing an "illegal" action.
    public void testExceptions(Tester t){
        setBank();
        t.checkException("Test for invalid Checking withdraw",
                         new RuntimeException("1000 is not available"),
                         this.check1,
                         "withdraw",
                         1000);
        t.checkException("Test for invalid Credit deposit", 
            new RuntimeException("300 is the maximum amount that can be funded to this account"), 
            this.credit1, 
            "deposit", 
            1000);
        t.checkException("Test for invalid account number", 
            new RuntimeException("Account 12 does not exist"), 
            this.testBank, 
            "deposit", 12, 200); 
        t.checkException("Test for invalid Credit deposit through Bank", 
            new RuntimeException("300 is the maximum amount that can be funded to this account"), 
            this.testBank, 
            "deposit", 10, 500); 
        t.checkException("Test for invalid account number during a remove request", 
            new RuntimeException("Account 12 does not exist"), 
            this.testBank, 
            "remove", 12);
    }
    
    // Test the deposit method(s)
    public void testDeposit(Tester t){
        reset();
        t.checkExpect(check1.withdraw(25), 75);
        t.checkExpect(check1, new Checking(1, 75, "First Checking Account", 20));
        reset();
        t.checkExpect(check1.deposit(50), 150);
        t.checkExpect(check1, new Checking(1, 150, "First Checking Account", 20));
    }
    
    // Test add for opening an account with a bank
    public void testAddAccount(Tester t) {
      reset();
      testBank.add(check1);
      MutableLoA ans = new MutableLoA();
      ans.add(check1);
      t.checkExpect(testBank.accounts, ans);
    }
    
    public void testBankDeposit(Tester t) {
      setBank();
      testBank.deposit(10, 200);
      t.checkExpect(testBank.findAccountByNum(10), new Credit(10, 100, "First Credit Account", 1000, 4.5));
    }
    
    public void testRemoveAccount(Tester t) {
      reset();
      testBank.add(check1);
      testBank.add(check2);
      MutableLoA ans2 = new MutableLoA();
      ans2.add(check1);
      testBank.remove(2);
      t.checkExpect(testBank.accounts, ans2);
    }
}
