import java.util.ArrayList;
import tester.*;

class ExampleHeaps {
  
  ArrayList<Integer> uhl, l, al; // for unheapedList
  ArrayList<Integer> hl; // for heapedList
  ArrayUtils au;
  
  void initList() {
    l = new ArrayList<Integer>();
    l.add(1);
    l.add(2);
    l.add(3);
    l.add(4);
    l.add(5);
    l.add(6);
    l.add(7);
    l.add(8);
    l.add(9);
    al = new ArrayList<Integer>();
    al.add(1);
    al.add(2);
    al.add(3);
    al.add(4);
    al.add(5);
    al.add(6);
    al.add(7);
    al.add(8);
    al.add(9);
  }
  
  void testBuild(Tester t) {
    uhl = new ArrayList<Integer>();
    hl = new ArrayList<Integer>(); 
    au = new ArrayUtils();
    uhl.add(10);
    uhl.add(20);
    uhl.add(20);
    uhl.add(15);
    uhl.add(30);
    uhl.add(40);
    uhl.add(50);
    uhl.add(50);
    uhl.add(60);
    uhl.add(80);
    hl.add(80);
    hl.add(60);
    hl.add(50);
    hl.add(30);
    hl.add(50);
    hl.add(40);
    hl.add(20);
    hl.add(10);
    hl.add(20);
    hl.add(15);
    au.buildHeap(uhl,  new GreaterThan());
    t.checkExpect(au.sorted(uhl, new GreaterThan()), false);
    t.checkExpect(au.validHeap(uhl, new GreaterThan()), true);
//    t.checkExpect(uhl, new ArrayList<Integer>());
    au.heapSort(uhl, new GreaterThan());
//    t.checkExpect(uhl, new ArrayList<Integer>());
    t.checkExpect(au.sorted(uhl, new GreaterThan()), true);
    initList();
    au.buildHeap(l, new GreaterThan());
    au.buildHeapAlt(al, new GreaterThan());
    t.checkExpect(l, al);
  }
}

class ArrayUtils {
  ArrayUtils() {}
  
  public <T> void swap(ArrayList<T> arr, int index1, int index2) {
    T oldValueAtIndex2 = arr.get(index2);
    
    arr.set(index2, arr.get(index1));
    arr.set(index1, oldValueAtIndex2);
  }
  
  public <T> void heapSort(ArrayList<T> arr, IComparator<T> comp) {
    buildHeap(arr, comp);
    int i;
    for (i = arr.size() - 1;
        i >= 0;
        i = i - 1) {
      removeMax(arr, comp, i);
    }
  }
  
  public <T> void buildHeap(ArrayList<T> arr, IComparator<T> comp) {
    int i;
    for (i = 1;
        i < arr.size();
        i = i + 1) {
      upheap(arr, comp, i);
    }
  }
  
  public <T> void buildHeapAlt(ArrayList<T> arr, IComparator<T> comp) {
    int i;
    for (i = ((arr.size() - 1) / 2);
        i >= 0;
        i = i - 1) {
      downheap(arr, comp, i, arr.size() - 1);
    }
  }
  
  public <T> void upheap(ArrayList<T> arr, IComparator<T> comp, int idx) {
    if (idx == 0) { return; }
    else {
      int parentIdx = Math.floorDiv((idx - 1), 2);
      if (comp.compare(arr.get(idx), arr.get(parentIdx)) > 0) {
        swap(arr, idx, parentIdx);
        upheap(arr, comp, parentIdx);
      }
    }
  }
  
  public <T> void removeMax(ArrayList<T> arr, IComparator<T> comp, int heapSize) {
    swap(arr, 0, heapSize);
    downheap(arr, comp, 0, heapSize - 1);
  }
  
  public <T> void downheap(ArrayList<T> arr, IComparator<T> comp, int idx, int heapSize) {
    int leftIdx = 2 * idx + 1;
    int rightIdx = 2 * idx + 2;
    if (rightIdx <= heapSize) {
      if (comp.compare(arr.get(rightIdx), arr.get(idx)) > 0 || comp.compare(arr.get(leftIdx), arr.get(idx)) > 0) {
        int biggestIdx;
        if (comp.compare(arr.get(rightIdx), arr.get(leftIdx)) >= 0) {
          biggestIdx = rightIdx;
        }
        else {
          biggestIdx = leftIdx;
        }
        swap(arr, idx, biggestIdx);
        downheap(arr, comp, biggestIdx, heapSize);
      }
    }
    else if (leftIdx <= heapSize) {
      if (comp.compare(arr.get(leftIdx), arr.get(idx)) > 0) {
        int biggestIdx = leftIdx;
        swap(arr, idx, biggestIdx);
      }
    }
    
  }
  
  public <T> boolean validHeap(ArrayList<T> arr, IComparator<T> comp) {
    int i;
    for (i = Math.floorDiv((arr.size() - 1), 2);
        i >= 0;
        i = i - 1) {
      if (i * 2 + 2 <= arr.size() - 1) { 
        if (i * 2 + 1 < arr.size() - 1) {
          if (comp.compare(arr.get(i * 2 + 2), arr.get(i)) > 0 ||
              comp.compare(arr.get(i * 2 + 1), arr.get(i)) > 0) {
            return false;
          }
        }
        else if (i * 2 + 1 < arr.size() - 1) {
          if (comp.compare(arr.get(i * 2 + 1), arr.get(i)) > 0) {
            return false;
          }
        }
      }
    }
    return true;
  }
  
  public <T> boolean sorted(ArrayList<T> arr, IComparator<T> comp) {
    int i;
    for (i = 0;
        i < arr.size() - 2;
        i = i + 1) {
      if (comp.compare(arr.get(i), arr.get(i + 1)) > 0) {
        return false;
      }
    }
    return true;
  }
}

class GreaterThan implements IComparator<Integer> {
  
  public int compare(Integer i, Integer j) {
    return i - j;
  }
}