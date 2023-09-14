import tester.*;

class Author {
  String last;
  String first;
  Author(String last, String first) {
    this.last = last;
    this.first = first;
  }
  
  boolean equals(Author that) {
    return this.last.equals(that.last) &&
        this.first.equals(that.first);
  }
}

class Book {
  String title;
  Author author;
  Book(String title, Author author) {
    this.title = title;
    this.author = author;
  }
  
  boolean sameBook(Book that) {
    return this.title.equals(that.title) &&
        this.author.equals(that.author);
  }
}

class CartPt {
  int x;
  int y;
  CartPt(int x, int y) {
    this.x = x;
    this.y = y;
  }
  
  boolean samePoint(CartPt that) {
    return this.x == that.x &&
        this.y == that.y;
  }
}

interface IShape {
  
  boolean sameShape(IShape that);
 //is this shape a Circle?
  boolean sameCircle(Circle that);
 // is this shape a Rect?
  boolean sameRect(Rect that);
 // is this shape a Square?
  boolean sameSquare(Square that);
  
}

abstract class AShape implements IShape {
  
  public abstract boolean sameShape(IShape that);
  
//is this shape a Circle?
  public boolean sameCircle(Circle that) {
    return false;
  }
 // is this shape a Rect?
  public boolean sameRect(Rect that) {
    return false;
  }
 // is this shape a Square?
  public boolean sameSquare(Square that) {
    return false;
  }
  
}
class Circle extends AShape {
  int x, y;
  int radius;
  Circle(int x, int y, int radius) {
    this.x = x;
    this.y = y;
    this.radius = radius;
  }
  
  public boolean sameCircle(Circle that) {
    return this.x == that.x &&
        this.y == that.y &&
        this.radius == that.radius;
  }
  
  public boolean sameShape(IShape that) {
      return that.sameCircle(this);
  }
  
}
class Rect extends AShape {
  int x, y;
  int w, h;
  Rect(int x, int y, int w, int h) {
    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
  }
  
  public boolean sameRect(Rect that) {
    return this.x == that.x &&
        this.y == that.y &&
        this.w == that.w &&
        this.h == that.h;
  }

  public boolean sameShape(IShape that) {
    return that.sameRect(this);
  }
  
}

class Square extends Rect {
  int x, y;
  int s;
  Square(int x, int y, int s) {
    super(x, y, s, s);
  }
  
  public boolean sameSquare(Square that) {
    return this.x == that.x &&
        this.y == that.y &&
        this.s == that.s;
  }
  
  //is this shape a Rect?
  public boolean sameRect(Rect that) {
    return false;
  }

  public boolean sameShape(IShape that) {
    return that.sameSquare(this);
  }
  
}

class ExampleEquals {
 
  CartPt pt1 = new CartPt(1,2);
  CartPt pt2 = new CartPt(1,2);
  CartPt pt3 = new CartPt(2,3);
  Book b1 = new Book("FartBook", new Author("String", "Shit"));
  
  boolean testSamePt(Tester t) {
    return
        t.checkExpect(this.pt1.samePoint(pt2), true) &&
        t.checkExpect(this.pt1.samePoint(pt3), false) &&
        t.checkExpect(this.b1.sameBook(b1), true);
  }
  
  Circle c1 = new Circle(3, 4, 5);
  Circle c2 = new Circle(4, 5, 6);
  Circle c3 = new Circle(3, 4, 5);
  Rect r1 = new Rect(3, 4, 5, 5);
  Rect r2 = new Rect(4, 5, 6, 7);
  Rect r3 = new Rect(3, 4, 5, 5);
  Square s1 = new Square(3,4,5);
  
  boolean testSameShape(Tester t) {
    return 
        t.checkExpect(this.r1.sameShape(c1), false) &&
        t.checkExpect(this.r1.sameShape(r1), true) &&
        t.checkExpect(this.s1.sameShape(r1), false) &&
        t.checkExpect(this.r1.sameShape(s1), false);
  }
  
}