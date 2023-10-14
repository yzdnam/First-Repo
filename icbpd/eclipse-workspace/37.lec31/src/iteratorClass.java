import java.util.*;

//Represents the ability to produce a sequence of values
//of type T, one at a time
//interface Iterator<T> {
//Does this sequence have at least one more value?
//  boolean hasNext();
//Get the next value in this sequence
//EFFECT: Advance the iterator to the subsequent value
//  T next();
//EFFECT: Remove the item just returned by next()
//NOTE: This method may not be supported by every iterator; ignore it for now
// void remove();
//}

//Represents anything that can be iterated over
//interface Iterable<T> {
//Returns an iterator over this collection
//  Iterator<T> iterator();
//}

class IListIterator<T> implements Iterator<T> {
  IList<T> items;
  IListIterator(IList<T> items) {
    this.items = items;
  }
  
  public boolean hasNext() {
    return this.items.isCons();
  }
  
  public T next() {
    ConsList<T> itemsAsCons = this.items.asCons();
    T answer = itemsAsCons.first;
    this.items = itemsAsCons.rest;
    return answer;
  }
  
  public void remove() {
    throw new UnsupportedOperationException("Don't do this!");
  }
}