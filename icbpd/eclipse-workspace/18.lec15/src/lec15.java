import tester.*;
import javalib.worldimages.*;   // images, like RectangleImage or OverlayImages
import javalib.funworld.*;      // the abstract World class and the big-bang library
import java.awt.Color;          // general colors (as triples of red,green,blue values)
                                // and predefined colors (Color.RED, Color.GRAY, etc.)
import javalib.worldcanvas.*;
import java.util.Random;

class Examples {
  IList<Circle> circs = new ConsList<Circle>(new Circle(3, 3, 5),
      new MtList<Circle>());
  IList<Double> circPerims = circs.map(new CirclePerimeter());
  
  Book htdp = new Book("HtDP", "MF", 0.0, 2014);
  Book hp = new Book("HP & the search for more money", "JKR", 9000.00, 2015);
  Book gatsby = new Book("The Great Gatsby", "FSF", 15.99, 1930);
  IList<Book> mtList = new MtList<Book>();
  IList<Book> twoBooks = new ConsList<Book>(this.htdp, 
                         new ConsList<Book>(this.hp, 
                             this.mtList));
  IList<Book> threeBooks = new ConsList<Book>(this.gatsby, this.twoBooks);
  
  boolean testFoldr(Tester t) {
    return
        t.checkInexact(threeBooks.foldr(new TotalPrice(), 0.0), 9015.99, 0.000001);
  }
}

class Utils {
  Double totalPrice(IList<Book> books) {
    return books.foldr(new TotalPrice(), 0.0);
  }
}

interface IPred<T> {
  boolean apply(T t);
}

interface IComparator<T> {
  int compare(T t1, T t2);
}

interface IFunc<A, R> {
  R apply(A arg);
}

class CirclePerimeter implements IFunc<Circle, Double> {
  public Double apply(Circle c) { return 2 * c.rad * Math.PI; }
}

class RunnerName implements IFunc<Runner, String> {
  public String apply(Runner r) { return r.name; }
}

interface IFunc2<A1, A2, R> {
  R apply(A1 arg1, A2 arg2);
}

class TotalPrice implements IFunc2<Book, Double, Double> {
  public Double apply(Book b, Double sum) { return b.price + sum; }
}

interface IList<T> {
  IList<T> filter(IPred<T> pred);
  IList<T> sort(IComparator<T> comp);
  int length();
  IList<T> insert(IComparator<T> comp, T t);
  <U> IList<U> map(IFunc<T, U> f);
  <U> U foldr(IFunc2<T, U, U> f, U base);
}

class MtList<T> implements IList<T> {
  public IList<T> filter(IPred<T> pred) { return this; }
  public IList<T> sort(IComparator<T> comp) { return this; }
  public int length() { return 0; }
  public IList<T> insert(IComparator<T> comp, T t) { return new ConsList<T>(t, this); }
  public <U> IList<U> map(IFunc<T, U> f) { return new MtList<U>(); }
  public <U> U foldr(IFunc2<T, U, U> f, U base) { return base; }
}

class ConsList<T> implements IList<T> {
  T first;
  IList<T> rest;
  ConsList(T first, IList<T> rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public <U> IList<U> map(IFunc<T, U> f) {
    return new ConsList<U>(f.apply(this.first), this.rest.map(f));
  }
  
  public IList<T> filter(IPred<T> pred) {
    if (pred.apply(this.first)) {
      return new ConsList<T>(this.first, this.rest.filter(pred));
    }
    else {
      return this.rest.filter(pred);
    }
  }
  
  public int length() {
    return 1 + this.rest.length();
  }
  
  public IList<T> sort(IComparator<T> comp) {
    return this.rest.sort(comp).insert(comp, this.first);
  }
  
  public IList<T> insert(IComparator<T> comp, T t) {
    if (comp.compare(this.first, t) > 0) {
      return new ConsList<T>(this.first, this.rest.insert(comp, t));
    }
    else return new ConsList<T>(t, this);
  }
  
  public <U> U foldr(IFunc2<T, U, U> f, U base) {
    return f.apply(first, rest.foldr(f, base));
  }
}

interface IShape { }

//to represent a circle
class Circle implements IShape {
 int x;
 int y;
 int rad;

 Circle(int x, int y, int rad) {
     this.x = x;
     this.y = y;
     this.rad = rad;
 }
}

//to represent a rectangle
class Rect implements IShape {
 int x;
 int y;
 int w;
 int h;

 Rect(int x, int y, int w, int h) {
     this.x = x;
     this.y = y;
     this.w = w;
     this.h = h;
 }
}

class Runner {
  String name;
  int age;
  int bib;
  boolean isMale;
  int pos;
  int time;
  Runner(String name, int age, int bib, boolean isMale, int pos, int time) {
    this.name = name;
    this.age = age;
    this.bib = bib;
    this.isMale = isMale;
    this.pos = pos;
    this.time = time;
  } 
}

class RunnerIsInFirst50 implements IPred<Runner> {
  public boolean apply(Runner r) { return r.pos <= 50; }
}


interface IRunnerPredicate {
  boolean apply(Runner r);
}

interface IRunnerComparator {
  int compare(Runner r1, Runner r2);
}


class Author {
  String last;
  String first;
  Author(String last, String first) {
    this.last = last;
    this.first = first;
  }
}

interface IAuthorPredicate {
  boolean apply(Author a);
}

interface IAuthorComparator {
  int compare(Author a1, Author a2);
}

class Book {
  String name;
  String author;
  double price;
  int year;
  Book(String name, String author, double price, int year) {
      this.name = name;
      this.author = author;
      this.price = price;
      this.year = year;
  }
}