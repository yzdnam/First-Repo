import java.util.ArrayList;

interface IComparator<T> {
  int compare(T t1, T t2);
}

class CompareCells implements IComparator<Cell> {
  public int compare(Cell c1, Cell c2) {
    if (c1.x > c2.x) { return 1; }
    else if (c1.x < c2.x) { return -1; }
    else if (c1.y > c2.y) { return 1; }
    else if (c1.y < c2.y) { return -1; }
    else { return 0; }
  }
}

class ArrayUtils {
  <T> int binarySearch(ArrayList<T> arr, T target, IComparator<T> comp) {
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
}