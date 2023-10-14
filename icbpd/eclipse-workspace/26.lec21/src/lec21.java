import tester.*;
import java.util.ArrayList;

class ExampleArrayLists {
  void testAdd(Tester t) {
    ArrayList<String> someStrings = new ArrayList<String>();
    someStrings.add("First string");
    someStrings.add("Second string");
    t.checkExpect(someStrings.get(0), "First string");
    t.checkExpect(someStrings.get(1), "Second string");
 
    // Insert this item at index 1, and move everything else back
    someStrings.add(1, "Squeezed in");
    t.checkExpect(someStrings.get(0), "First string");
    t.checkExpect(someStrings.get(1), "Squeezed in");
    t.checkExpect(someStrings.get(2), "Second string");
  }
  
  void testSwap(Tester t) {
    ArrayList<String> someStrings = new ArrayList<String>();
    someStrings.add("Second string");
    someStrings.add("First string");
   
    new ArrayUtils().swap(someStrings, 0, 1);
   
    t.checkExpect(someStrings.get(0), "First string");
    t.checkExpect(someStrings.get(1), "Second string");
  }
  
  void testRecurFoldL(Tester t) {
    ArrayList<String> strings = new ArrayList<String>();
    strings.add("a");
    strings.add("b");
    strings.add("c");
    ArrayList<String> e = new ArrayList<String>();
    e.add("a");
    t.checkExpect(new ArrayUtils().recurFoldl(strings, new Concat(), ""), "cba");
    t.checkExpect(new ArrayUtils().recurFoldr(strings, new Concat(), ""), "abc");
    t.checkExpect(new ArrayUtils().foldl(strings, new Concat(), ""), "cba");
    t.checkExpect(new ArrayUtils().foldr(strings, new Concat(), ""), "abc");
    }
  
  void testBinarySearch(Tester t) {
    ArrayList<String> fruits = new ArrayList<String>();
    fruits.add("apple");
    fruits.add("banana");
    fruits.add("date");
    t.checkExpect(new ArrayUtils().gen_binarySearch_v2(fruits, "cherry", new CompareStrings()), -1);
  }
  
  void testInterLeave(Tester t) {
    ArrayList<Integer> t1 = new ArrayList<Integer>();
    ArrayList<Integer> t2 = new ArrayList<Integer>();
    ArrayList<Integer> t3 = new ArrayList<Integer>();
    ArrayList<Integer> t4 = new ArrayList<Integer>();
    t1.add(1);
    t2.add(2);
    t1.add(3);
    t3.add(1);
    t3.add(2);
    t3.add(3);
    t4.add(1);
    t4.add(3);
    t4.add(2);
    t.checkExpect(new ArrayUtils().interleave(t1, t2), t3);
    t.checkExpect(new ArrayUtils().unshuffle(t3), t4);
  }
  
  void testMinValIdx(Tester t) {
    ArrayList<String> newfruits = new ArrayList<String>();
    newfruits.add("banana");
    newfruits.add("apple");
    newfruits.add("date");
    t.checkExpect(new ArrayUtils().minValIdx(newfruits, new CompareStrings()), 1);
  }
  
  void testBuildList(Tester t) {
    ArrayList<Integer> buildListTest = new ArrayList<Integer>();
    buildListTest.add(0);
    buildListTest.add(1);
    buildListTest.add(4);
    t.checkExpect(new ArrayUtils().buildList(3, new Sqr()), buildListTest);
  }
  
}

class ArrayUtils {
  // EFFECT: Exchanges the values at the given two indices in the given array
  <T> void swap(ArrayList<T> arr, int index1, int index2) {
    T oldValueAtIndex2 = arr.get(index2);
   
    arr.set(index2, arr.get(index1));
    arr.set(index1, oldValueAtIndex2);
  }
  
  <T, U> ArrayList<U> map(ArrayList<T> arr, IFunc<T, U> func) {
    ArrayList<U> result = new ArrayList<U>();
    return this.mapHelp(arr, func, 0, result);
  }
  
  <T, U> ArrayList<U> mapHelp(ArrayList<T> source, IFunc<T, U> func,
      int curIdx, ArrayList<U> dest) {
    if (curIdx >= source.size()) {
      return dest;
    }
    else {
      dest.add(func.apply(source.get(curIdx)));
      return this.mapHelp(source, func, curIdx + 1, dest);
    }
  }
  
  <X, Z> Z recurFoldl(ArrayList<X> arr, IFunc2<X, Z, Z> func, Z base) {
    return this.recurFoldlHelp(arr, func, base, 0);
  }
  
  <X, Z> Z recurFoldlHelp(ArrayList<X> arr, IFunc2<X, Z, Z> func, Z base, int curIdx) {
    if (curIdx >= arr.size()) {
      return base;
    }
    else {
      return recurFoldlHelp(arr, func, func.apply(arr.get(curIdx), base), curIdx + 1);
    }
  }
  
  <X, Z> Z recurFoldr(ArrayList<X> arr, IFunc2<X, Z, Z> func, Z base) {
    return this.recurFoldrHelp(arr, func, base, arr.size() - 1);
  }
  
  <X, Z> Z recurFoldrHelp(ArrayList<X> arr, IFunc2<X, Z, Z> func, Z base, int curIdx) {
    if (curIdx < 0) {
      return base;
    }
    else {
      return recurFoldrHelp(arr, func, func.apply(arr.get(curIdx), base), curIdx - 1);
    }
  }
  
  <X, Z> Z foldl(ArrayList<X> arr, IFunc2<X, Z, Z> func, Z base) {
    Z result = base;
    for (X x : arr) {
      result = func.apply(x, result);
    }
    return result;
  }
  
  <X, Z> Z foldr(ArrayList<X> arr, IFunc2<X, Z, Z> func, Z base) {
    Z result = base;
    X next;
    int curIdx = 1;
    for (X x : arr) {
      next = arr.get(arr.size() - curIdx);
      result = func.apply(next, result);
      curIdx = curIdx + 1;
    }
    return result;
  }
  
//In ArrayUtils
  <T> int gen_binarySearch_v2(ArrayList<T> arr, T target, IComparator<T> comp) {
    return this.gen_binarySearchHelp_v2(arr, target, comp, 0, arr.size());
  }
  <T> int gen_binarySearchHelp_v2(ArrayList<T> arr, T target, IComparator<T> comp,
      int lowIdx, int highIdx) {
    int midIdx = (lowIdx + highIdx) / 2;
    if (lowIdx >= highIdx) {
      return -1;
    }
    else if (comp.compare(target, arr.get(midIdx)) == 0) {
      return midIdx;
    }
    else if (comp.compare(target, arr.get(midIdx)) > 0) {
      return this.gen_binarySearchHelp_v2(arr, target, comp, midIdx + 1, highIdx);
    }
    else {
      return this.gen_binarySearchHelp_v2(arr, target, comp, lowIdx, midIdx);
    }
  }
  
  <T> ArrayList<T> interleave(ArrayList<T> arr1, ArrayList<T> arr2) {
    ArrayList<T> result = new ArrayList<T>();
    for (int idx = 0;
        idx < arr1.size() || idx < arr2.size();
        idx = idx + 1)
    {
      if (idx >= arr1.size()) {
        result.add(arr2.get(idx));
      }
      else if (idx >= arr2.size()) {
        result.add(arr1.get(idx));
      }
      else {
        result.add(arr1.get(idx));
        result.add(arr2.get(idx));
      }
    }
    return result;
  }
  
  <T> ArrayList<T> unshuffle(ArrayList<T> arr) {
    ArrayList<T> result = new ArrayList<T>();
    for (int idx = 0;
        idx < arr.size();
        idx = idx + 2) {
      result.add(arr.get(idx));
    }
    for (int idx = 1;
        idx < arr.size();
        idx = idx + 2) {
      result.add(arr.get(idx));
    }
    return result;
  }
  
  <T> int minValIdx(ArrayList<T> arr, IComparator<T> comp) {
    int result = 0;
    for (int idx = 0;
        idx < arr.size();
        idx = idx + 1) {
      if (comp.compare(arr.get(idx), arr.get(result)) < 0) {
        result = idx;
      }
    }
    return result;
  }

  <U> ArrayList<U> buildList(int n, IFunc<Integer, U> func) {
    ArrayList<U> result = new ArrayList<U>();
    for (int i = 0;
        i < n;
        i = i + 1) {
      result.add(func.apply(i));
    }
    return result;
  }
  
  ArrayList<Book> capitalizeTitles(ArrayList<Book> books) {
    int curIdx = 0;
    for (Book b : books) {
      Book oldBook = books.get(curIdx);
      String newTitle = oldBook.name.toUpperCase();
      Book newBook = new Book(newTitle, oldBook.author, oldBook.price, oldBook.year);
      books.set(curIdx, newBook);
    }
    return books;
  }
  
}

class Concat implements IFunc2<String, String, String> {
  public String apply(String s1, String s2) { return s1 + s2; }
}

class CompareStrings implements IComparator<String> {
  public int compare(String s1, String s2) { return s1.compareTo(s2); }
}

class Sqr implements IFunc<Integer, Integer> {
  public Integer apply(Integer i) { return i * i; }
}
