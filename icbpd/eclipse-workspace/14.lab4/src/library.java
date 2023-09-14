import tester.*;

interface IBook {
  
  int daysOverdue(int day);
  boolean isOverdue(int day);
  double computeFine(int day);
  
}

abstract class ABook implements IBook{
  String title;
  int dayTaken;
  ABook(String title, int dayTaken) {
    this.title = title;
    this.dayTaken = dayTaken;
  }
  
  public abstract int daysOverdue(int day);
  
  public boolean isOverdue(int day) {
    return this.daysOverdue(day) > 0;
  }
  
  public double computeFine(int day) {
    return this.daysOverdue(day) * 0.1; 
  }
  
}

class RefBook extends ABook {
  RefBook(String title, int dayTaken) {
    super(title, dayTaken);
  }
  
  public int daysOverdue(int day) {
    return day - this.dayTaken + 2;
  }
  
}

abstract class NotRefBook extends ABook {
  String author;
  NotRefBook(String title, String author, int dayTaken) {
    super(title, dayTaken);
    this.author = author;
  }
  
  public int daysOverdue(int day) {
    return day - this.dayTaken + 14; 
  }
}

class Book extends NotRefBook {
  Book(String title, String author, int dayTaken) {
    super(title, author, dayTaken);
  }
}



class AudioBook extends NotRefBook {
  AudioBook(String title, String author, int dayTaken) {
    super(title, author, dayTaken);
  }
  
  public double computeFine(int day) {
    return this.daysOverdue(day) * 0.2; 
  }

}