import java.util.HashMap;

class Book {
  Author author;
  String title;
  int year;
  public int hashCode() {
    return this.author.hashCode() * 10000 + this.year;
  }
  
  public boolean equals(Object other) {
    if (!(other instanceof Book)) { return false; }
    // this cast is safe, because we just checked instanceof
    Book that = (Book)other;
    return this.author.equals(that.author)
        && this.year == that.year
        && this.title.equals(that.title);
  }
}
class Author {
  Book book;
  String name;
  int yob;
  public int hashCode() {
    return this.name.hashCode() * 10000 + this.yob;
  }
  
  public boolean equals(Object other) {
    if (!(other instanceof Author)) { return false; }
    // this cast is safe, because we just checked instanceof
    Author that = (Author)other;
    return this.name.equals(that.name)
        && this.yob == that.yob;
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
  
  public boolean equals(Object o) {
    if (!(o instanceof AShape)) { return false; }
    IShape that = (IShape)o;
    return this.sameShape(that);
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
  public int hashCode() {
    return x * y * radius;
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
  public int hashCode() {
    return x * y * w * h;
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
  
  public int hashCode() {
    return x * y * s;
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