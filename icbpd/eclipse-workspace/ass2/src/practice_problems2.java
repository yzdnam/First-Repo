import tester.*;

interface ILog {
  double miles();
  ILog oneMonth(int month, int year);
  ILog logsBetween(Date from, Date to);
  double milesInMonth(int month, int year);
  double longestRun();
  double longestRunBetween(Date from, Date to);
  boolean allRunsShorterThan(double miles);
}

class MTLog implements ILog{
  MTLog() {}
  
  public double miles() {
    return 0.0;
  }
  
  public ILog oneMonth(int month, int year) {
    return this;
  }
  public ILog logsBetween(Date from, Date to) {
    return this;
  }
  public double milesInMonth(int month, int year) {
    return 0.0;
  }
  public double longestRun() {
    return 0.0;
  }
  public double longestRunBetween(Date from, Date to) {
    return 0.0;
  }
  public boolean allRunsShorterThan(double miles) {
    return true;
  }
  
}


class ConsLog implements ILog {
  Entry fst;
  ILog rst;
  ConsLog(Entry fst, ILog rst) {
    this.fst = fst;
    this.rst = rst;
  }
  
  public double miles() {
    return
        this.fst.distance + this.rst.miles();
  }
  
  public ILog oneMonth(int month, int year) {
    if (this.fst.sameMonthandYear(month, year)) {
      return
          new ConsLog(this.fst, this.rst.oneMonth(month, year));
    }
    else {return this.rst.oneMonth(month, year);}
  }
  
  public ILog logsBetween(Date from, Date to) {
    if (this.fst.betweenDates(from, to)) {
      return
          new ConsLog(this.fst, this.rst.logsBetween(from, to));
    }
    else { return this.rst.logsBetween(from, to); }
  }
  
  public double milesInMonth(int month, int year) {
    return this.oneMonth(month, year).miles();
  }
  
  public double longestRun() {
    if (this.fst.distance > this.rst.longestRun()) {
      return this.fst.distance;
    }
    else { return this.rst.longestRun(); }
  }
  
  public double longestRunBetween(Date from, Date to) {
    return this.logsBetween(from, to).longestRun();
  }
  
  public boolean allRunsShorterThan(double miles) {
    if (this.fst.distance >= miles) {
      return false;
    }
    else {return this.rst.allRunsShorterThan(miles);}
  }
  
}

class Entry {
  Date d;
  double distance; // miles
  int duration; // minutes
  String comment;
  Entry(Date d, double distance, int duration, String comment) {
    this.d = d;
    this.distance = distance;
    this.duration = duration;
    this.comment = comment;
  }
  
  boolean sameMonthandYear(int mon, int yr) {
    return this.d.month == mon && this.d.year == yr;
  }
  
  boolean betweenDates(Date d1, Date d2) {
    if (this.d.year == d1.year && this.d.month == d1.month) {
      return this.d.day >= d1.day;
    }
    else if (this.d.year == d2.year && this.d.month == d2.month) {
      return this.d.day <= d2.day;
    }
    else if (this.d.year == d1.year) {
      return this.d.month > d1.month;
    }
    else if (this.d.year == d2.year ) {
      return this.d.month < d2.month;
    }
    else { return this.d.year > d1.year && this.d.year < d2.year; }
  }
}

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

class CompositeExamples {
  Date d1 = new Date(5,5,2003);
  Date d2 = new Date(6,6,2003);
  Date d3 = new Date(23,6,2003);
  Date d4 = new Date(23,6,2007);
  
  Entry e1 = new Entry(d1,5.0,25,"Good");
  Entry e2 = new Entry(d2,3.0,24,"Tired");
  Entry e3 = new Entry(d3,26.0,156,"Great");
  Entry e4 = new Entry(d4,30.0,10,"Good");
  
  ILog l1 = new MTLog();
  ILog l2 = new ConsLog(e1,l1);
  ILog l3 = new ConsLog(e2,l2);
  ILog l4 = new ConsLog(e3,l3);
  ILog l5 = new ConsLog(e4,l4);
  
  boolean testMiles(Tester t) {
    return
        t.checkInexact(this.l1.miles(), 0.0, 0.1) &&
        t.checkInexact(this.l2.miles(), 5.0, 0.1) &&
        t.checkInexact(this.l3.miles(), 8.0, 0.1) &&
        t.checkInexact(this.l4.miles(), 34.0, 0.1);
  }
  
  boolean testOneMonth(Tester t) {
    return
        t.checkExpect(this.l1.oneMonth(6,2003), this.l1) &&
        t.checkExpect(this.l3.oneMonth(6,2003), new ConsLog(this.e2, this.l1)) &&
        t.checkExpect(this.l4.oneMonth(6,2003), new ConsLog(this.e3, new ConsLog(this.e2, this.l1)));
  }
  
  boolean testLongestBetween(Tester t) {
    return
        t.checkExpect((this.l5.longestRunBetween(new Date(1,5, 2003), new Date(22, 6, 2003))), 5.0) &&
        t.checkExpect((this.l5.longestRunBetween(new Date(1, 5, 2003) , new Date(24,6,2003))), 26.0);
  }
  
  boolean testMilesInMonth(Tester t) {
    return
        t.checkExpect(this.l4.milesInMonth(5, 2003), 5.0) &&
        t.checkExpect(this.l4.milesInMonth(6, 2003), 29.0);
  }
}