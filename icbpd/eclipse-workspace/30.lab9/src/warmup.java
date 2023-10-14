import java.util.ArrayList;
import tester.*;
import javalib.impworld.*;
import javalib.worldimages.*;
import java.awt.Color;

class WarmupTests {
  ArrayList<String> strs;
  
  void initArray() {
    strs = new ArrayList<String>();
    strs.add("bull");
    strs.add("el");
  }
  
  void testFilter(Tester t) {
    initArray();
    ArrayList<String> newstrs = new Utils().filter(strs, new StartsWith("e"));
    t.checkExpect(newstrs.contains("el"), true);
    t.checkExpect(newstrs.contains("bull"), false);
    t.checkExpect(newstrs.size(), 1);
  }
  void testRemoveExcept(Tester t) {
    initArray();
    new Utils().removeExcept(strs, new StartsWith("e"));
    t.checkExpect(strs.contains("el"), true);
    t.checkExpect(strs.contains("bull"), false);
    t.checkExpect(strs.size(), 1);
  }
}

class Utils {
  Utils() {}
  
  <T> ArrayList<T> filter(ArrayList<T> arr, IPred<T> pred) {
    ArrayList<T> ans = new ArrayList<T>();
    for (T t: arr) {
      if (pred.apply(t)) {
        ans.add(t);
      }
    }
    return ans;
  }
  
  <T> void removeExcept(ArrayList<T> arr, IPred<T> pred) {
    ArrayList<T> ans = new ArrayList<T>();
    for (T t: arr) {
      if (!pred.apply(t)) {
        ans.add(t);
      }
    }
    for (T t: ans) {
      arr.remove(t);
    }
  }
}