
// represents a list of Person's buddies
interface ILoBuddy {
  public boolean contains(Person that);
  public int countCommonBuddies(Person that);
  public boolean hasExtendedBuddyHelper(Person that, ILoBuddy checkedPersons);
  public ILoBuddy allBuddies(ILoBuddy checkedBuddy);
  public int length();
}