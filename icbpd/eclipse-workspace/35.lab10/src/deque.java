abstract class AVertex<T> {
  AVertex<T> next;
  AVertex<T> prev;
  AVertex(AVertex<T> next, AVertex<T> prev) {
    this.next = next;
    this.prev = prev;
  }
  
  abstract int sizeHelper(int accum);
  
  abstract T getData(AVertex<T> Vertex);
  
  void insert(T addition, AVertex<T> newNext, AVertex<T> newPrev) {
    new Vertex<T>(addition, newNext, newPrev);
  }
  
  abstract T remove(AVertex<T> Vertex);
  
  abstract AVertex<T> findHelper(IPred<T> pred);
  
  abstract void removeVertex();
  
}

class Vertex<T> extends AVertex<T> {
  T data;
  
  Vertex(T data) {
    super(null, null);
    this.data = data;
  }
  
  Vertex(T data, AVertex<T> next, AVertex<T> prev) {
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
  
  public T getData(AVertex<T> Vertex) { 
    return this.data;
  }
  
  public T remove(AVertex<T> Vertex) {
    this.next.prev = this.prev;
    this.prev.next = this.next;
    return this.data;
  }
  
  public AVertex<T> findHelper(IPred<T> pred) {
    if (pred.apply(this.data)) {
      return this;
    }
    else return this.next.findHelper(pred);
  }
  
  public void removeVertex() {
    this.remove(this);
  }
}

class Sentinel<T> extends AVertex<T> {
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
  
  T remove(AVertex<T> Vertex) {
    if (Vertex == this) {
      throw new RuntimeException("Attempting to remove from an empty list");
    }
    else {
       return Vertex.remove(this);
    }
  }
  
  public T getData(AVertex<T> Vertex) {
    if (Vertex == this) {
      throw new RuntimeException("getData() called on empty list");
    }
    else return Vertex.getData(this);
  }
  
  AVertex<T> find(IPred<T> pred) {
    return this.next.findHelper(pred);
  }
  
  public AVertex<T> findHelper(IPred<T> pred) {
    return this;
  }
  
  public void removeVertex() {
    return;
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
  
  AVertex<T> find(IPred<T> pred) {
    return header.find(pred);
  }
  
  void removeVertex(AVertex<T> Vertex) {
    Vertex.removeVertex();
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