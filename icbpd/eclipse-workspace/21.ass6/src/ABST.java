import tester.*;

class Examples {
  Book htdp = new Book("HtDP", "MF", 0);
  Book hp = new Book("HP & the search for more money", "JKR", 9000);
  Book gatsby = new Book("The Great Gatsby", "FSF", 15);
  Leaf<Book> titleLeaf = new Leaf<Book>(new BooksByTitle());
  Leaf<Book> authorLeaf = new Leaf<Book>(new BooksByAuthor());
  Leaf<Book> priceLeaf = new Leaf<Book>(new BooksByPrice());
  
  ABST<Book> byTitle = new Vertex<Book>(new BooksByTitle(), hp, 
      titleLeaf,
      new Vertex<Book>(new BooksByTitle(), htdp, titleLeaf,
          new Vertex<Book>(new BooksByTitle(), gatsby, titleLeaf, titleLeaf)));
  
  IList<Book> byTitleList = new ConsList<Book>(hp, 
      new ConsList<Book>(htdp, 
          new ConsList<Book>(gatsby, 
              new MtList<Book>())));
  
  ABST<Book> byAuthor = new Vertex<Book>(new BooksByAuthor(), hp,
      new Vertex<Book>(new BooksByAuthor(), gatsby, authorLeaf, authorLeaf),
      new Vertex<Book>(new BooksByAuthor(), htdp, authorLeaf, authorLeaf));
  
  IList<Book> byAuthorList = new ConsList<Book>(gatsby, 
      new ConsList<Book>(hp, 
          new ConsList<Book>(htdp, 
              new MtList<Book>())));
  
  ABST<Book> byPrice = new Vertex<Book>(new BooksByPrice(), gatsby, 
      new Vertex<Book>(new BooksByPrice(), htdp, priceLeaf, priceLeaf),
      new Vertex<Book>(new BooksByPrice(), hp, priceLeaf, priceLeaf));
  
  IList<Book> byPriceList = new ConsList<Book>(htdp,
      new ConsList<Book>(gatsby,
          new ConsList<Book>(hp,
              new MtList<Book>())));
  
  ABST<Book> badByTitle = new Vertex<Book>(new BooksByTitle(), htdp,
      new Vertex<Book>(new BooksByTitle(), hp, titleLeaf,
          new Vertex<Book>(new BooksByTitle(), gatsby, titleLeaf, titleLeaf)), titleLeaf);
  
  ABST<Book> badByAuthor = new Vertex<Book>(new BooksByAuthor(), hp,
      new Vertex<Book>(new BooksByAuthor(), gatsby, authorLeaf, 
          new Vertex<Book>(new BooksByAuthor(), htdp, authorLeaf, authorLeaf)), authorLeaf);
  
  Book wp = new Book("War and Peace", "LT", 18);
  
  ABST<Book> byTitleV2 = new Vertex<Book>(new BooksByTitle(), hp, 
      titleLeaf,
      new Vertex<Book>(new BooksByTitle(), htdp, titleLeaf,
          new Vertex<Book>(new BooksByTitle(), gatsby, titleLeaf, 
              new Vertex<Book>(new BooksByTitle(), wp, titleLeaf, titleLeaf))));
  
  ABST<Book> byPriceV2 = new Vertex<Book>(new BooksByPrice(), gatsby, 
      new Vertex<Book>(new BooksByPrice(), htdp, priceLeaf, priceLeaf),
      new Vertex<Book>(new BooksByPrice(), hp, 
          new Vertex<Book>(new BooksByPrice(), wp, priceLeaf, priceLeaf), priceLeaf));
  
  IList<Book> byPriceV2List = new ConsList<Book>(htdp, 
      new ConsList<Book>(gatsby, 
          new ConsList<Book>(wp, 
              new ConsList<Book>(hp, 
                  new MtList<Book>()))));
  
  boolean testInsert(Tester t) {
    return t.checkExpect(byTitle.insert(wp), new Vertex<Book>(new BooksByTitle(), hp, 
        titleLeaf,
        new Vertex<Book>(new BooksByTitle(), htdp, titleLeaf,
            new Vertex<Book>(new BooksByTitle(), gatsby, titleLeaf, 
                new Vertex<Book>(new BooksByTitle(), wp, titleLeaf, titleLeaf))))) &&
        t.checkExpect(byAuthor.insert(wp), new Vertex<Book>(new BooksByAuthor(), hp,
            new Vertex<Book>(new BooksByAuthor(), gatsby, authorLeaf, authorLeaf),
            new Vertex<Book>(new BooksByAuthor(), htdp, 
                new Vertex<Book>(new BooksByAuthor(), wp, authorLeaf, authorLeaf), authorLeaf))) &&
        t.checkExpect(byPrice.insert(wp), new Vertex<Book>(new BooksByPrice(), gatsby, 
            new Vertex<Book>(new BooksByPrice(), htdp, priceLeaf, priceLeaf),
            new Vertex<Book>(new BooksByPrice(), hp, 
                new Vertex<Book>(new BooksByPrice(), wp, priceLeaf, priceLeaf), priceLeaf)));
  }
  
  boolean testPresent(Tester t) {
    return
        t.checkExpect(byTitle.present(gatsby), true) &&
        t.checkExpect(byPrice.present(wp), false);
  }
  
  boolean testGetLeft(Tester t) {
    return
        t.checkExpect(byTitle.getLeftMost(), hp) &&
        t.checkExpect(byAuthor.getLeftMost(), gatsby);
  }
  
  boolean testGetRight(Tester t) {
    return
        t.checkExpect(byTitle.getRight(), 
            new Vertex<Book>(new BooksByTitle(), htdp, titleLeaf,
                new Vertex<Book>(new BooksByTitle(), gatsby, titleLeaf, titleLeaf))) &&
        t.checkExpect(byAuthor.getRight(), 
            new Vertex<Book>(new BooksByAuthor(), hp, authorLeaf, 
                new Vertex<Book>(new BooksByAuthor(), htdp, authorLeaf, authorLeaf)));
  }
  
  boolean testSameTree(Tester t) {
    return t.checkExpect(byTitle.sameTree(byTitle), true) &&
        t.checkExpect(byTitle.sameTree(byAuthor), false);
  }
  
  boolean testSameData(Tester t) {
    return t.checkExpect(byAuthor.sameData(byTitle), true) &&
        t.checkExpect(byTitle.sameData(byAuthor), true) &&
        t.checkExpect(byTitle.sameData(byTitleV2), false) &&
        t.checkExpect(byTitleV2.sameData(byTitle), false);
  }
  
  boolean testBuildList(Tester t) {
    return t.checkExpect(byPriceV2.buildList(), byPriceV2List);
  }
}  

interface Comparator<T> {
  int compare(T t1, T t2);
}

class BooksByTitle implements Comparator<Book> {
  public int compare(Book b1, Book b2) {
   return b1.title.compareTo(b2.title);
  }
}

class BooksByAuthor implements Comparator<Book> {
  public int compare(Book b1, Book b2) {
    return b1.author.compareTo(b2.author);
  }
}

class BooksByPrice implements Comparator<Book> {
  public int compare(Book b1, Book b2) {
    return b1.price - b2.price;
  }
}

abstract class ABST<T> {
  Comparator<T> order;
  
  ABST(Comparator<T> order) {
    this.order = order;
  }
  
  public abstract ABST<T> insert(T item);
  
  public abstract boolean present(T item);
  
  public abstract T getLeftMost();
  
  public abstract T getLeftMostHelper(Vertex<T> parentVertex);
  
  public abstract ABST<T> getRight();
  
  public abstract ABST<T> getRightHelper(Vertex<T> parentVertex);
  
  public abstract boolean sameTree(ABST<T> other);
  
  public boolean sameLeaf(ABST<T> other) { return false; }
  
  public boolean sameVertex(ABST<T> other) { return false; }
  
  public boolean sameData(T other) { return false; }
  
  public boolean sameLeft(ABST<T> other) { return false; }
  
  public boolean sameRight(ABST<T> other) { return false; }
  
  public abstract boolean sameData(ABST<T> other);
  
  public abstract boolean sameDataHelper(ABST<T> other);
  
  public abstract IList<T> buildList();
    
}

class Leaf<T> extends ABST<T> {
  Leaf(Comparator<T> order) {
    super(order);
  }
  
  public ABST<T> insert(T item) {
    return new Vertex<T>(order, item, this, this);
  }
  
  public boolean present(T item) {
    return false;
  }
  
  public T getLeftMost() {
    throw new RuntimeException("No leftmost item of an empty tree");
  }
  
  public T getLeftMostHelper(Vertex<T> parentVertex) {
    return parentVertex.data;
  }
  
  public ABST<T> getRight() {
    throw new RuntimeException("No right of an empty tree");
  }
  
  public ABST<T> getRightHelper(Vertex<T> parentVertex) {
    return parentVertex.right;
  }
  
  public boolean sameTree(ABST<T> other) {
    return other.sameLeaf(this);
  }
  
  public boolean sameLeaf(ABST<T> other) {
    return true;
  }
  
  public boolean sameData(ABST<T> other) {
    return true; 
  }
  
  public boolean sameDataHelper(ABST<T> other) {
    return true; 
  }
  
  public IList<T> buildList() {
    return new MtList<T>();
  }
  
}

class Vertex<T> extends ABST<T> {
  T data;
  ABST<T> left;
  ABST<T> right;
  Vertex(Comparator<T> order, T data, ABST<T> left, ABST<T> right) {
    super(order);
    this.data = data;
    this.left = left;
    this.right = right;
  }
  
  public ABST<T> insert(T item) {
    if (order.compare(data, item) > 0) {
      return new Vertex<T>(order, data, left.insert(item), right);
    }
    else
      return new Vertex<T>(order, data, left, right.insert(item));
  }
  
  public boolean present(T item) {
    return (order.compare(data, item) == 0) || left.present(item) || right.present(item);
  }
  
  public T getLeftMost() {
    return left.getLeftMostHelper(this);
  }
  
  public T getLeftMostHelper(Vertex<T> parentVertex) {
    return left.getLeftMostHelper(this);
  }
  
  public ABST<T> getRight() {
    return left.getRightHelper(this);
  }
  
  public ABST<T> getRightHelper(Vertex<T> parentVertex) {
    return new Vertex<T>(parentVertex.order, parentVertex.data, this.getRight(), parentVertex.right);
  }
  
  public boolean sameTree(ABST<T> other) {
    return other.sameVertex(this);
  }
  
  public boolean sameVertex(ABST<T> other) {
    return other.sameData(this.data) && other.sameLeft(this.left) && other.sameRight(this.right); 
  }
  
  public boolean sameData(T other) {
    return this.data.equals(other);
  }
  
  public boolean sameLeft(ABST<T> other) {
    return this.left.sameTree(other);
  }
  
  public boolean sameRight(ABST<T> other) {
    return this.right.sameTree(other);
  }
  
  public boolean sameData(ABST<T> other) {
    return other.present(this.data) && 
        this.left.sameDataHelper(other) && 
        this.right.sameDataHelper(other) && 
        other.sameDataHelper(this);
  }
  
  public boolean sameDataHelper(ABST<T> other) {
    return other.present(this.data) &&
        this.left.sameDataHelper(other) &&
        this.right.sameDataHelper(other);
  }
  
  public IList<T> buildList() {
    return new ConsList<T>(this.getLeftMost(), this.getRight().buildList());
  }
  
}

class Book {
  String title;
  String author;
  int price;
  Book(String title, String author, int price) {
    this.title = title;
    this.author = author;
    this.price = price;
  }
  
  public boolean equals(Book other) {
    return this.title.equals(other.title) && 
        this.author.equals(other.author) && 
        this.price == other.price;
  }
}