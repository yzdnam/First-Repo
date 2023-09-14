import tester.*;

class Utils {
  int checkRange(int val, int min, int max, String msg) {
    if (val >= min && val <= max) {
      return val;
    }
    else {
      throw new IllegalArgumentException(msg);
    }
  }
}

class Date {
  int month;
  int day;
  int year;
  Date(int year, int month, int day) {
    this.year = new Utils().checkRange(year, 1500, 2100,
      "Invalid year: " + Integer.toString(year));
    this.month = new Utils().checkRange(month, 1, 12,
      "Invalid month " + Integer.toString(month));
    this.day = new Utils().checkRange(day, 1, 31,
      "Invalid day: " + Integer.toString(day));
  }
}

class ExampleDates {
  
  //Good dates
  Date d20100228 = new Date(2010, 2, 28);   // Feb 28, 2010
  Date d20091012 = new Date(2009, 10, 12);  // Oct 12, 2009
  
  boolean testCheckRange (Tester t) {
    return
        t.checkException(new IllegalArgumentException("exception"), new Utils(), "checkRange", 1, 2, 3, "exception");
  }

}