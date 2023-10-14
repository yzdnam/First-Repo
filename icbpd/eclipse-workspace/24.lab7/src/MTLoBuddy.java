
// represents an empty list of Person's buddies
class MTLoBuddy implements ILoBuddy {
    MTLoBuddy() {}

    public boolean contains(Person that) { return false; }
    
    public int countCommonBuddies(Person that) { return 0; }
    
    public boolean hasExtendedBuddyHelper(Person that, ILoBuddy checkedPersons) {
      return false;
    }

    public ILoBuddy allBuddies(ILoBuddy checkedBuddies) {
      return checkedBuddies;
    }
    
    public ILoBuddy append(ILoBuddy other) {
      return other;
    }
    
    public int length() {
      return 0;
    }
}
