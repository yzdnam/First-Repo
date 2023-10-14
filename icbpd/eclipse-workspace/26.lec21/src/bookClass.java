import tester.*;
import javalib.worldimages.*;   // images, like RectangleImage or OverlayImages
import javalib.funworld.*;      // the abstract World class and the big-bang library
import java.awt.Color;          // general colors (as triples of red,green,blue values)
                                // and predefined colors (Color.RED, Color.GRAY, etc.)
import javalib.worldcanvas.*;

import java.util.ArrayList;
import java.util.Random;

class Examples {
  IList<Circle> circs = new ConsList<Circle>(new Circle(3, 3, 5),
      new MtList<Circle>());
  IList<Double> circPerims = circs.map(new CirclePerimeter());
  
  Book htdp = new Book("HtDP", "MF", 0.0, 2014);
  Book htdpCAPS = new Book("HTDP", "MF", 0.0, 2014);
  Book hp = new Book("HP & the search for more money", "JKR", 9000.00, 2015);
  Book gatsby = new Book("The Great Gatsby", "FSF", 15.99, 1930);
  IList<Book> mtList = new MtList<Book>();
  IList<Book> twoBooks = new ConsList<Book>(this.htdp, 
                         new ConsList<Book>(this.hp, 
                             this.mtList));
  IList<Book> threeBooks = new ConsList<Book>(this.gatsby, this.twoBooks);
  
  void testCapitalizeTitles(Tester t) {
    ArrayList<Book> bookT1 = new ArrayList<Book>();
    bookT1.add(htdp);
    ArrayList<Book> bookA1 = new ArrayList<Book>();
    bookA1.add(htdpCAPS);
    t.checkExpect(new ArrayUtils().capitalizeTitles(bookT1), bookA1);
  }
  
}

class Utils {
  Double totalPrice(IList<Book> books) {
    return books.foldr(new TotalPrice(), 0.0);
  }
}


class CirclePerimeter implements IFunc<Circle, Double> {
  public Double apply(Circle c) { return 2 * c.rad * Math.PI; }
}

class RunnerName implements IFunc<Runner, String> {
  public String apply(Runner r) { return r.name; }
}

class TotalPrice implements IFunc2<Book, Double, Double> {
  public Double apply(Book b, Double sum) { return b.price + sum; }
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