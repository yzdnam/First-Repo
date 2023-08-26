// Problem 2.4

import tester.Tester;

class Automobile {
  String model;
  int price; // in dollars
  double mileage; // in mpg
  boolean used;
  Automobile(String model, int price, double mileage, boolean used) {
    this.model = model;
    this.price = price;
    this.mileage = mileage;
    this.used = used;
  }
}


// Problem 3.1 
class House {
  String type;
  int rooms;
  int asking_price;
  String street;
  String city;
  House(String type, int rooms, int asking_price, String street, String city) {
    this.type = type;
    this.rooms = rooms;
    this.asking_price = asking_price;
    this.street = street;
    this.city = city;
  }
}

// Problem 5.3
interface ILoHouse {}

class MtLoHouse implements ILoHouse {
  MtLoHouse() {
  }
}

class ConsLoHouse implements ILoHouse {
  House first;
  ILoHouse rest;
  ConsLoHouse(House first, ILoHouse rest) {
    this.first = first;
    this.rest = rest;
  }
}

// Problem 4.4
interface BankAccount {}

class CheckingAccount implements BankAccount {
  String name;
  int id;
  int cur_bal;
  int min_bal;
  CheckingAccount(String name, int id, int cur_bal, int min_bal) {
    this.name = name;
    this.id = id;
    this.cur_bal = cur_bal;
    this.min_bal = min_bal;
  }
}

class SavingsAccount implements BankAccount {
  String name;
  int id;
  int cur_bal;
  double int_rate;
  SavingsAccount(String name, int id, int cur_bal, double int_rate) {
    this.name = name;
    this.id = id;
    this.cur_bal = cur_bal;
    this.int_rate = int_rate;
  }
}

class CD implements BankAccount {
  String name;
  int id;
  int cur_bal;
  double int_rate;
  String maturity;
  CD(String name, int id, int cur_bal, double int_rate, String maturity) {
    this.name = name;
    this.id = id;
    this.cur_bal = cur_bal;
    this.int_rate = int_rate;
    this.maturity = maturity;
  }
}

// Problem 10.6
class Image {
  int width; // in pixels
  int height; // in pixels
  String source;

  Image(int width, int height, String source) {
    this.width = width;
    this.height = height;
    this.source = source;
  }
  
  int pixel_count() {
    return this.width * this.height;
  }
  
  String sizeString() {
    int pixels = this.pixel_count();
    if (pixels <= 10000) {
      return "small";
    }
    else if (pixels < 1000000) {
      return "medium";
    }
    else return "large";
  }
}

// Problem 11.2
class Date {
  int day;
  int month;
  int year;
  Date(int day, int month, int year) {
    this.day = day;
    this.month = month;
    this.year = year;
  }
}

class TempRange {
  int high;
  int low;
  TempRange(int high, int low) {
    this.high = high;
    this.low = low;
  }
  
  // determines whether the current range is within the given range
  boolean within(TempRange range) {
    return this.high <= range.high && this.low >= range.low;
  }
  
  boolean broke(TempRange range) {
    return this.high > range.high || this.low < range.low;
  }
}

class WeatherRecord {
  Date d;
  TempRange today;
  TempRange normal;
  TempRange record;
  double pcent_precip; 
  WeatherRecord(Date d, TempRange today, TempRange normal, TempRange record, double pcent_precip) {
    this.d = d;
    this.today = today;
    this.normal = normal;
    this.record = record;
    this.pcent_precip = pcent_precip;
  }
  
  boolean withinRange() {
    return this.today.within(normal);
  }
  
  // determines whether the chance of precipitation is higher than some given value
  boolean rainyDay(double val) {
    return this.pcent_precip > val;
  }
  
  boolean recordDay() {
    return this.today.broke(record);
  }
}

// Problem 14.7
interface Products {
  double unitPrice();
  boolean lowerUnitPrice(double val);
  boolean cheaperThan(Products p);
}

class PracticeIceCream implements Products {
  String brand;
  int weight; // in grams
  int price; // in cents
  String flavor;
  PracticeIceCream(String brand, int weight, int price, String flavor) {
    this.brand = brand;
    this.weight = weight;
    this.price = price;
    this.flavor = flavor;
  }
  
  public double unitPrice() {
    return this.price / this.weight;
  }
  public boolean lowerUnitPrice(double val) {
    return this.unitPrice() < val;
  }
  public boolean cheaperThan(Products p) {
    return this.lowerUnitPrice(p.unitPrice());
  }
}

class Coffee implements Products {
  String brand;
  int weight;
  int price;
  String caffeinated;
  Coffee(String brand, int weight, int price, String caffeinated) {
    this.brand = brand;
    this.weight = weight;
    this.price = price;
    this.caffeinated = caffeinated;
  }
  public double unitPrice() {
    return this.price / this.weight;
}
  public boolean lowerUnitPrice(double val) {
    return this.unitPrice() < val;
  }
  public boolean cheaperThan(Products p) {
    return this.lowerUnitPrice(p.unitPrice());
  }
}

class Juice implements Products {
  String brand;
  int weight;
  int price;
  String flavor;
  String pkging;
  Juice(String brand, int weight, int price, String flavor, String pkging) {
    this.brand = brand;
    this.weight = weight;
    this.price = price;
    this.flavor = flavor;
    this.pkging = pkging;
  }
  public double unitPrice() {
    return this.price / this.weight;
}
  public boolean lowerUnitPrice(double val) {
    return this.unitPrice() < val;
  }
  public boolean cheaperThan(Products p) {
    return this.lowerUnitPrice(p.unitPrice());
  }
}

class PracticeExamples {
  Automobile chevy93 = new Automobile("1500", 1500, 100000, true);
  
  House brookline_ranch = new House("Ranch", 7, 375000, "23 Maple Street", "Brookline");
  House col_newton = new House("Colonial", 9, 450000, "5 Joye Road", "Newton");
  House cape_waltham = new House("Cape", 6, 235000, "83 Winslow Road", "Waltham");
  ILoHouse mtList = new MtLoHouse();
  
  ILoHouse list1 = new ConsLoHouse(brookline_ranch, 
                       new ConsLoHouse (col_newton,
                           new ConsLoHouse(cape_waltham, mtList)));
  
  CheckingAccount earl_gray = new CheckingAccount("Earl Gray", 1729, 1250, 500);
  CD ima_flatt = new CD("Ima Flatt", 4104, 10123, 4, "June 1, 2005");
  SavingsAccount annie_proulx = new SavingsAccount("Annie Proulx", 2992, 800, 3.5);
  
  Image testimage = new Image(100, 1000, "fart");
  
  boolean testsizeString(Tester t ) {
    return
        t.checkExpect(testimage.sizeString(), "medium");
  }
}