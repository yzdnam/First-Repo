// represents a list of Person's buddies
class ConsLoBuddy implements ILoBuddy {

    Person first;
    ILoBuddy rest;

    ConsLoBuddy(Person first, ILoBuddy rest) {
        this.first = first;
        this.rest = rest;
    }
    
    public boolean contains(Person that) {
      if (this.first.username.equals(that.username)) {
        return true;
      }
      else return this.rest.contains(that);
    }
    
    public int countCommonBuddies(Person that) {
      if (that.hasDirectBuddy(this.first)) {
        return 1 + this.rest.countCommonBuddies(that);
      }
      else return this.rest.countCommonBuddies(that);
    }
    
    public boolean hasExtendedBuddyHelper(Person that, ILoBuddy checkedPersons) {
      if (checkedPersons.contains(this.first)) {
        return this.rest.hasExtendedBuddyHelper(that, checkedPersons);
      }
      else if (this.first.hasExtendedBuddyHelper(that, new ConsLoBuddy(this.first, checkedPersons))) { return true; }
      else return this.rest.hasExtendedBuddyHelper(that, new ConsLoBuddy(this.first, checkedPersons));
    }
    
    public ILoBuddy allBuddies(ILoBuddy checkedBuddies) {
      if (checkedBuddies.contains(this.first)) {
        return this.rest.allBuddies(checkedBuddies);
      }
      else return
          this.rest.allBuddies(this.first.buddies.allBuddies(new ConsLoBuddy(this.first, checkedBuddies)));
      }
    
    public int length() {
      return 1 + this.rest.length();
    }

}
