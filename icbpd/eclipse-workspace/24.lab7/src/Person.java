
// represents a Person with a user name and a list of buddies
class Person {

    String username;
    ILoBuddy buddies;

    Person(String username) {
        this.username = username;
        this.buddies = new MTLoBuddy();
    }
    
 
    // Change this person's buddy list so that it includes the given person
    void addBuddy(Person buddy){
      this.buddies = new ConsLoBuddy(buddy, this.buddies);
    }

    // returns true if this Person has that as a direct buddy
    boolean hasDirectBuddy(Person that) {
        return this.buddies.contains(that);
    }

    // returns the number of people who will show up at the party 
    // given by this person
    int partyCount() {
        return this.allBuddies().length() - 1;
    }

    ILoBuddy allBuddies() {
      return this.buddies.allBuddies(new ConsLoBuddy(this, new MTLoBuddy()));
    }

    // returns the number of people that are direct buddies 
    // of both this and that person
    int countCommonBuddies(Person that) {
        return that.buddies.countCommonBuddies(this);
    }
    
    // will the given person be invited to a party 
    // organized by this person?
    boolean hasExtendedBuddy(Person that) {
      if (!this.hasDirectBuddy(that)) {
        return this.buddies.hasExtendedBuddyHelper(that, new ConsLoBuddy(this, new MTLoBuddy()));
      }
      else return true;
    }
    
    public boolean hasExtendedBuddyHelper(Person that, ILoBuddy checkedPersons) {
      if (!this.hasDirectBuddy(that)) {
        return this.buddies.hasExtendedBuddyHelper(that, checkedPersons);
      }
      else return true;
    }

}
