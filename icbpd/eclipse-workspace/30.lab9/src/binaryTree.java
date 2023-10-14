import tester.*;

interface IBinaryTree<T> {
  boolean isVertex();
  BTVertex<T> asVertex();
}

class ExampleBTs {
  IBinaryTree<Character> bt = new BTVertex<Character>('a', 
      new BTVertex<Character>('b', new BTVertex<Character>('d', new Leaf<Character>(), new Leaf<Character>()), 
          new BTVertex<Character>('e', new Leaf<Character>(), new Leaf<Character>())),
      new BTVertex<Character>('c', new BTVertex<Character>('f', new Leaf<Character>(), new Leaf<Character>()), 
          new BTVertex<Character>('g', new Leaf<Character>(), new Leaf<Character>())));
  
  PostOrderIterator<Character> po;
  InOrderIterator<Character> io;
  
  void initIterators() {
    po = new PostOrderIterator<Character>(bt);
    io = new InOrderIterator<Character>(bt);
  }
  
  void testIterators(Tester t) {
    initIterators();
    t.checkExpect(po.next(), 'd');
    t.checkExpect(po.next(), 'e');
    t.checkExpect(po.next(), 'b');
    t.checkExpect(po.next(), 'f');
    t.checkExpect(po.next(), 'g');
    
    t.checkExpect(io.next(), 'd');
    t.checkExpect(io.next(), 'b');
    t.checkExpect(io.next(), 'e');
    t.checkExpect(io.next(), 'a');
    t.checkExpect(io.next(), 'f');
  }
}

class BTVertex<T> implements IBinaryTree<T> {
  T data;
  IBinaryTree<T> left;
  IBinaryTree<T> right;
  BTVertex(T data, IBinaryTree<T> left, IBinaryTree<T> right) {
    this.data = data;
    this.left = left;
    this.right = right;
  }
  
  public boolean isVertex() { return true; }
  public BTVertex<T> asVertex() { return this; }
}

class Leaf<T> implements IBinaryTree<T> {
  Leaf() {}
  
  public boolean isVertex() { return false; }
  public BTVertex<T> asVertex() { throw new IllegalArgumentException("asVertex() called with a Leaf"); }
}

class BreadthFirstIterator<T> implements Iterator<T> {
  Deque<IBinaryTree<T>> worklist;
  BreadthFirstIterator(IBinaryTree<T> source) {
    this.worklist = new Deque<IBinaryTree<T>>();
    this.addIfNotLeaf(source);
  }
  // EFFECT: only adds the given binary-tree if it's not a leaf
  void addIfNotLeaf(IBinaryTree<T> bt) {
    if (bt.isVertex()) {
      this.worklist.addAtTail(bt);
    }
  }
  public boolean hasNext() {
    // we have a next item if the worklist isn't empty
        return this.worklist.size() > 0;
  }
  public T next() {
    // Get (and remove) the first item on the worklist --
    // and we know it must be a BTVertex
        BTVertex<T> Vertex = this.worklist.removeFromHead().asVertex();
        // Add the children of the Vertex to the tail of the list
        this.addIfNotLeaf(Vertex.left);
        this.addIfNotLeaf(Vertex.right);
        // return the answer
        return Vertex.data;
  }
  public void remove() {
    throw new UnsupportedOperationException("Don't do this!");
  }
}

class PreOrderIterator<T> implements Iterator<T> {
  Deque<IBinaryTree<T>> worklist;
  PreOrderIterator(IBinaryTree<T> source) {
    this.worklist = new Deque<IBinaryTree<T>>();
    this.addIfNotLeaf(source);
  }
  // EFFECT: only adds the given binary-tree if it's not a leaf
  void addIfNotLeaf(IBinaryTree<T> bt) {
    if (bt.isVertex()) {
      this.worklist.addAtHead(bt);
    }
  }
  public boolean hasNext() {
    // we have a next item if the worklist isn't empty
        return this.worklist.size() > 0;
  }
  public T next() {
    // Get (and remove) the first item on the worklist --
    // and we know it must be a BTVertex
        BTVertex<T> Vertex = this.worklist.removeFromHead().asVertex();
        // Add the children of the Vertex to the tail of the list
        this.addIfNotLeaf(Vertex.right);
        this.addIfNotLeaf(Vertex.left);
        // return the answer
        return Vertex.data;
  }

  
  public void remove() {
    throw new UnsupportedOperationException("Don't do this!");
  }
}


class PostOrderIterator<T> implements Iterator<T> {
  Deque<IBinaryTree<T>> worklist;
  PostOrderIterator(IBinaryTree<T> source) {
    this.worklist = new Deque<IBinaryTree<T>>();
    this.addIfNotLeaf(source);
  }
  // EFFECT: only adds the given binary-tree if it's not a leaf
  void addIfNotLeaf(IBinaryTree<T> bt) {
    if (bt.isVertex()) {
      this.worklist.addAtHead(bt);
      addIfNotLeaf(bt.asVertex().right);
      addIfNotLeaf(bt.asVertex().left);
    }
  }
  public boolean hasNext() {
    // we have a next item if the worklist isn't empty
        return this.worklist.size() > 0;
  }
  public T next() {
    // Get (and remove) the first item on the worklist --
    // and we know it must be a BTVertex
        BTVertex<T> Vertex = this.worklist.removeFromHead().asVertex();
        // return the answer
        return Vertex.data;
  }
  
  public void remove() {
    throw new UnsupportedOperationException("Don't do this!");
  }
}

class InOrderIterator<T> implements Iterator<T> {
  Deque<IBinaryTree<T>> worklist;
  InOrderIterator(IBinaryTree<T> source) {
    this.worklist = new Deque<IBinaryTree<T>>();
    this.addIfNotLeaf(source);
  }
  // EFFECT: only adds the given binary-tree if it's not a leaf
  void addIfNotLeaf(IBinaryTree<T> bt) {
    if (bt.isVertex()) {
      addIfNotLeaf(bt.asVertex().right);
      this.worklist.addAtHead(bt);
      addIfNotLeaf(bt.asVertex().left);
    }
  }
  public boolean hasNext() {
    // we have a next item if the worklist isn't empty
        return this.worklist.size() > 0;
  }
  public T next() {
    // Get (and remove) the first item on the worklist --
    // and we know it must be a BTVertex
        BTVertex<T> Vertex = this.worklist.removeFromHead().asVertex();
        // return the answer
        return Vertex.data;
  }
  
  public void remove() {
    throw new UnsupportedOperationException("Don't do this!");
  }
}
