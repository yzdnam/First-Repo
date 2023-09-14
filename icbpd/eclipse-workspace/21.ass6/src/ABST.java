import tester.*;

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
}

class Leaf<T> extends ABST<T> {
  Leaf(Comparator<T> order) {
    super(order);
  }
}

class Node<T> extends ABST<T> {
  T data;
  ABST<T> left;
  ABST<T> right;
  Node(Comparator<T> order, T data, ABST<T> left, ABST<T> right) {
    super(order);
    this.data = data;
    this.left = left;
    this.right = right;
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
}