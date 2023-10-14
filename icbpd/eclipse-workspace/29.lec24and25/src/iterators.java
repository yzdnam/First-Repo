import java.util.*;

class Utils {
  Utils() {}
  
  boolean getsToOne(int n) {
    ArrayList<Integer> numbersSeen = new ArrayList<Integer>();
    while (n > 1) {
      if (n % 2 == 0) {
        n = n / 2;
      }
      else {
        n = 3 * n + 1;
      }
      if (numbersSeen.contains(n)) { 
        return false; 
      }
      else {
        numbersSeen.add(n);
      }
    }
    return true;
  }
}

//Represents the ability to produce a sequence of values
//of type T, one at a time
interface Iterator<T> {
// Does this sequence have at least one more value?
  boolean hasNext();
// Get the next value in this sequence
// EFFECT: Advance the iterator to the subsequent value
  T next();
// EFFECT: Remove the item just returned by next()
//NOTE: This method may not be supported by every iterator; ignore it for now
  void remove();
}

//Represents anything that can be iterated over
interface Iterable<T> {
// Returns an iterator over this collection
  Iterator<T> iterator();
}

class ArrayListIterator<T> implements Iterator<T> {
  // the list of items that this iterator iterates over
  ArrayList<T> items;
  // the index of the next item to be returned
  int nextIdx;
  // Construct an iterator for a given ArrayList
  ArrayListIterator(ArrayList<T> items) {
    this.items = items;
    this.nextIdx = 0;
  }
 
  public boolean hasNext() {
    return nextIdx < items.size();
  }
 
  public T next() {
    T answer = this.items.get(this.nextIdx);
    this.nextIdx = this.nextIdx + 1;
    return answer;
  }
 
  public void remove() {
    throw new UnsupportedOperationException("Don't do this!");
  }
}

class IListIterator<T> implements Iterator<T> {
  // the list of items that this iterator iterates over
  IList<T> items;
  // Construct an iterator for a given IList
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

class DequeForwardIterator<T> implements Iterator<T> {
  Deque<T> items;
  AVertex<T> nextVertex;
  DequeForwardIterator(Deque<T> items) {
    this.items = items;
    this.nextVertex = items.header.next;
  }
  
  public boolean hasNext() {
    return this.nextVertex.isVertex();
  }
  
  public T next() {
    Vertex<T> nextAsVertex = this.nextVertex.asVertex();
    T answer = nextAsVertex.data;
    this.nextVertex = this.nextVertex.next;
    return answer;
  }
  
  public void remove() {
    nextVertex.prev.removeVertex();
  }
}

class DequeReverseIterator<T> implements Iterator<T> {
  Deque<T> items;
  AVertex<T> nextVertex;
  DequeReverseIterator(Deque<T> items) {
    this.items = items;
    this.nextVertex = items.header.prev;
  }
  
  public boolean hasNext() {
    return this.nextVertex.isVertex();
  }
  
  public T next() {
    Vertex<T> nextAsVertex = this.nextVertex.asVertex();
    T answer = nextAsVertex.data;
    this.nextVertex = this.nextVertex.prev;
    return answer;
  }
  
  public void remove() {
    nextVertex.next.removeVertex();
  }
}

class TakeN<T> implements Iterator<T> {
  Iterator<T> source;
  int howMany;
  TakeN(Iterator<T> source, int howMany) {
    this.source = source;
    this.howMany = howMany;
  }
  public boolean hasNext() {
    return howMany > 0 && this.source.hasNext();
  }
  
  public T next() {
    howMany = howMany - 1;
    return this.source.next();
  }
  
  public void remove() {
    this.source.remove();
  }
}

class InterleaveIterators<T> implements Iterator<T> {
  Iterator<T> source1;
  Iterator<T> source2;
  int count;
  InterleaveIterators(Iterator<T> source1, Iterator<T> source2) {
    this.source1 = source1;
    this.source2 = source2;
    this.count = 0;
  }
  public boolean hasNext() {
    if (count % 2 == 0) {
      return source1.hasNext();
    }
    else return source2.hasNext();
  }
  
  public T next() {
    if (count % 2 == 0) {
      count = count + 1;
      return source1.next();
    }
    else {
      count = count + 1;
      return source2.next();
    }
  }
  
  public void remove() {
    if (count % 2 == 0) {
      source2.remove();
    }
    else source1.remove();
  }
}