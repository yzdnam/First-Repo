abstract class ADequeNode<T> {
  ADequeNode<T> next;
  ADequeNode<T> prev;
  ADequeNode(ADequeNode<T> next, ADequeNode<T> prev) {
    this.next = next;
    this.prev = prev;
  }
  
  abstract int sizeHelper(int accum);
  
  abstract T getData(ADequeNode<T> DequeNode);
  
  void insert(T addition, ADequeNode<T> newNext, ADequeNode<T> newPrev) {
    new DequeNode<T>(addition, newNext, newPrev);
  }
  
  abstract T remove(ADequeNode<T> DequeNode);
  
  abstract ADequeNode<T> findHelper(IPred<T> pred);
  
  abstract void removeDequeNode();
  
  abstract boolean contains(T target);
  
}

class DequeNode<T> extends ADequeNode<T> {
  T data;
  
  DequeNode(T data) {
    super(null, null);
    this.data = data;
  }
  
  DequeNode(T data, ADequeNode<T> next, ADequeNode<T> prev) {
    super(next, prev);
    this.data = data;
    if (next == null || prev == null) {
      throw new IllegalArgumentException();
    }
    else {
      this.next.prev = this;
      this.prev.next = this;
    }
  }
  
  public int sizeHelper(int accum) {
    return this.next.sizeHelper(accum + 1);
  }
  
  public T getData(ADequeNode<T> DequeNode) { 
    return this.data;
  }
  
  public T remove(ADequeNode<T> DequeNode) {
    this.next.prev = this.prev;
    this.prev.next = this.next;
    return this.data;
  }
  
  public ADequeNode<T> findHelper(IPred<T> pred) {
    if (pred.apply(this.data)) {
      return this;
    }
    else return this.next.findHelper(pred);
  }
  
  public void removeDequeNode() {
    this.remove(this);
  }
  
  public boolean contains(T target) {
    if (this.data.equals(target)) {
      return true;
    }
    else return this.next.contains(target);
  }
}

class Sentinel<T> extends ADequeNode<T> {
  Sentinel() {
    super(null, null);
    this.next = this;
    this.prev = this;
  }
  
  int size() {
    return this.next.sizeHelper(0);
  }
  
  public int sizeHelper(int accum) {
    return accum;
  }
  
  T remove(ADequeNode<T> DequeNode) {
    if (DequeNode == this) {
      throw new RuntimeException("Attempting to remove from an empty list");
    }
    else {
       return DequeNode.remove(this);
    }
  }
  
  public T getData(ADequeNode<T> DequeNode) {
    if (DequeNode == this) {
      throw new RuntimeException("getData() called on empty list");
    }
    else return DequeNode.getData(this);
  }
  
  ADequeNode<T> find(IPred<T> pred) {
    return this.next.findHelper(pred);
  }
  
  public ADequeNode<T> findHelper(IPred<T> pred) {
    return this;
  }
  
  public void removeDequeNode() {
    return;
  }
  
  public boolean isEmpty() {
    return this.next == this;
  }
  
  public boolean contains(T target) {
    return false;
  }
  
}

class Deque<T> {
  Sentinel<T> header;
  
  Deque() {
    this.header = new Sentinel<T>();
  }
  
  Deque(Sentinel<T> header) {
  this.header = header;
  }
  
  int size() { return header.size(); }
  
  void addAtHead(T addition) {
    header.insert(addition, header.next, header);
  }
  
  void addAtTail(T addition) {
    header.insert(addition, header, header.prev);
  }
  
  T removeFromHead() {
    return header.remove(this.header.next);
  }
  
  T removeFromTail() {
    return header.remove(this.header.prev);
  }
  
  ADequeNode<T> find(IPred<T> pred) {
    return header.find(pred);
  }
  
  void removeDequeNode(ADequeNode<T> DequeNode) {
    DequeNode.removeDequeNode();
  }
  
  boolean isEmpty() {
    return header.isEmpty();
  }
  
  boolean contains(T target) {
    return header.next.contains(target);
  }
}

interface IPred<T> {
  boolean apply(T t);
}

class StartsWith implements IPred<String> {
  String prefix;
  StartsWith(String prefix) {
    this.prefix = prefix;
  }
  public boolean apply(String s) { return s.startsWith(prefix); }
}