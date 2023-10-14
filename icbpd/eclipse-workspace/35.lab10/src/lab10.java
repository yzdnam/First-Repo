import tester.*; 
import java.util.*;

class Examples {
  ArrayList<Integer> l, r;
  
  void init() {
    l = new ArrayList<Integer>();
    l.add(1);
    l.add(2);
    l.add(3);
    r = new ArrayList<Integer>();
    r.add(3);
    r.add(2);
    r.add(1);
  }
  
  void testRev(Tester t) {
    init();
    t.checkExpect(new Utils().reverse(l), r);
  }
  
  void testString(Tester t) {
    StringCreator creator = new StringCreator();
    t.checkExpect(creator.getString(),"");
    creator.add('c');
    creator.add('d');
    t.checkExpect(creator.getString(),"cd");
    creator.add('e');
    t.checkExpect(creator.getString(),"cde");
    creator.remove();
    creator.remove();
    t.checkExpect(creator.getString(),"c");
    creator.undo(); //undoes the removal of 'd'
    t.checkExpect(creator.getString(),"cd");
    creator.undo(); //undoes the removal of 'e'
    creator.undo(); //undoes the addition of 'e'
    t.checkExpect(creator.getString(),"cd");
    creator.add('a');
    t.checkExpect(creator.getString(),"cda");
    creator.undo(); //undoes the addition of 'a'
    creator.undo(); //undoes the addition of 'd'
    creator.undo(); //undoes the addition of 'c'
    t.checkExpect(creator.getString(),"");
    creator.undo(); //no effect, there is nothing to undo
  }
  
  void testListOfLists(Tester t) {
    ListOfLists<Integer> lol = new ListOfLists<Integer>();
    //add 3 lists
    lol.addNewList();
    lol.addNewList();
    lol.addNewList();
 
    //add elements 1,2,3 in first list
    lol.add(0,1);
    lol.add(0,2);
    lol.add(0,3);
 
    //add elements 4,5,6 in second list
    lol.add(1,4);
    lol.add(1,5);
    lol.add(1,6);
 
    //add elements 7,8,9 in third list
    lol.add(2,7);
    lol.add(2,8);
    lol.add(2,9);
 
    //iterator should return elements in order 1,2,3,4,5,6,7,8,9
    int number = 1;
    for (Integer num:lol) {
        t.checkExpect(num,number);
        number = number + 1;
    }
  }
}

class Runner {
  int age;
  String name;
  
  Runner(int age, String name){
    this.age = age;
    this.name = name;
  }
  
  public int hashCode() { 
    return this.name.hashCode() + this.age;
  }
  public boolean equals(Object o) {
    if (!(o instanceof Runner)) { return false; }
    Runner that = (Runner)o;
    return this.name.equals(that.name) &&
        this.age == that.age;
  }
}

class Stack<T> {
  Deque<T> contents;
  Stack() {
    this.contents = new Deque<T>();
  }
  void push(T item) { this.contents.addAtHead(item); } // adds an item to the stack
  
  boolean isEmpty() { return this.contents.size() == 0; }
  
  T pop() { return this.contents.removeFromHead(); } // removes and returns the top of the stack
}

class Utils {
  <T> ArrayList<T> reverse(ArrayList<T> source) {
    Stack<T> stack = new Stack<T>();
    ArrayList<T> result = new ArrayList<T>();
    for (T t : source) {
      stack.push(t);
    }
    while (!stack.isEmpty()) {
      result.add(stack.pop());
    }
    return result;
  }
}

class StringCreator {
  String str;
  Stack<String> strHist;
  StringCreator() {
    this.str = "";
    this.strHist = new Stack<String>();
  }

  void add(Character c) {
    strHist.push(str);
    str = str + c;
  }
  
  void remove() {
    if (str.length() == 0) { return; }
    else
      strHist.push(str);
      str = str.substring(0, str.length() - 1);
  }
  
  String getString() {
    return str;
  }
  
  void undo() {
    if (strHist.isEmpty()) { return; }
    else
      str = strHist.pop();
  }
}

class ListOfLists<T> implements Iterable<T> {
  ArrayList<ArrayList<T>> contents;
  ListOfLists() {
    this.contents = new ArrayList<ArrayList<T>>();
  }
  
  void addNewList() {
    ArrayList<T> addition = new ArrayList<T>();
    contents.add(addition);
  }
  
  void add(int index, T object) {
    contents.get(index).add(object);
  }
  
  ArrayList<T> get(int index) {
    return contents.get(index);
  }
  
  int size() {
    return contents.size();
  }
  
  public Iterator<T> iterator() {
    return new ListOfListsIterator<T>(this);
  }
}

class ListOfListsIterator<T> implements Iterator<T> {
  ListOfLists<T> items;
  int nextIdx;
  int currentListIdx;
  ListOfListsIterator(ListOfLists<T> items) {
    this.items = items;
    this.nextIdx = 0;
    this.currentListIdx = 0;
  }
  public boolean hasNext() {
    return nextIdx < items.get(currentListIdx).size() ||
        currentListIdx < items.size() - 1;
  }
  public T next() {
    if (nextIdx < items.get(currentListIdx).size()) {
      T answer = items.get(currentListIdx).get(nextIdx);
      nextIdx = nextIdx + 1;
      return answer;
    }
    else {
      currentListIdx = currentListIdx + 1;
      nextIdx = 0;
      T answer = items.get(currentListIdx).get(nextIdx);
      nextIdx = nextIdx + 1;
      return answer;
    }
  }
}