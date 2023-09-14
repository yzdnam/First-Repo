import tester.*;

interface IEntertainment {
    //compute the total price of this Entertainment
    double totalPrice();
    //computes the minutes of entertainment of this IEntertainment
    int duration();
    //produce a String that shows the name and price of this IEntertainment
    String format();
    //is this IEntertainment the same as that one?
    boolean sameEntertainment(IEntertainment that);
    boolean sameMagazine(Magazine that);
    boolean sameTVSeries(TVSeries that);
    boolean samePodcast(Podcast that);
}

abstract class AEntertainment implements IEntertainment {
  String name;
  double price;
  int installments;
  AEntertainment(String name, double price, int installments){
    this.name = name;
    this.price = price;
    this.installments = installments;
  }
  
  //computes the price of a yearly subscription to this Magazine
  public double totalPrice() {
      return this.price * this.installments;
  }
  
  //computes the minutes of entertainment of this Podcast
  public int duration() {
      return 50 * this.installments;
  }
  
  public abstract boolean sameEntertainment(IEntertainment that);
  
  public boolean sameMagazine(Magazine that) { return false; }
  public boolean sameTVSeries(TVSeries that) { return false; }
  public boolean samePodcast(Podcast that) { return false; }
  
  //produce a String that shows the name and price of this Magazine
  public String format() {
      return this.name + ", " + this.price + ".";
  }
  
}

class Magazine extends AEntertainment {
    String genre;
    int pages;
    
    Magazine(String name, double price, String genre, int pages, int installments) {
      super(name, price, installments);
      this.genre = genre;
      this.pages = pages;
    }
        
    //computes the minutes of entertainment of this Magazine, (includes all installments)
    public int duration() {
        return 5 * this.pages;
    }
    
    //is this Magazine the same as that IEntertainment?
    public boolean sameEntertainment(IEntertainment that) {
        return that.sameMagazine(this);
    }
    
    public boolean sameMagazine(Magazine that) {
      return this.name.equals(that.name) && Math.abs(this.price - that.price) <= 0.0001 &&
          this.installments == that.installments && this.genre.equals(that.genre) &&
          this.pages == that.pages;
    }

}

class TVSeries extends AEntertainment {
    String corporation;
    
    TVSeries(String name, double price, int installments, String corporation) {
      super(name, price, installments);
      this.corporation = corporation;
    }
    
    //is this TVSeries the same as that IEntertainment?
    public boolean sameEntertainment(IEntertainment that) {
        return that.sameTVSeries(this);
    }
    
    public boolean sameTVSeries(TVSeries that) {
      return this.name.equals(that.name) && Math.abs(this.price - that.price) <= 0.0001 &&
          this.installments == that.installments && this.corporation.equals(that.corporation);
    }
    
}

class Podcast extends AEntertainment {

    Podcast(String name, double price, int installments) {
      super(name, price, installments);
    }
    
    //is this Podcast the same as that IEntertainment?
    public boolean sameEntertainment(IEntertainment that) {
        return that.samePodcast(this);
    }
    
    public boolean samePodcast(Podcast that) {
      return this.name.equals(that.name) && Math.abs(this.price - that.price) <= 0.0001 &&
          this.installments == that.installments;
    }
    
}

class ExamplesEntertainment {
    IEntertainment rollingStone = new Magazine("Rolling Stone", 2.55, "Music", 60, 12);
    IEntertainment time = new Magazine("Time", 3.00, "Current Events", 60, 12);
    IEntertainment houseOfCards = new TVSeries("House of Cards", 5.25, 13, "Netflix");
    IEntertainment dopesick = new TVSeries("Dopesick", 6, 8, "Hulu");
    IEntertainment serial = new Podcast("Serial", 0.0, 8);
    IEntertainment jre = new Podcast("JRE", 0.0, 2248);
    
    //testing total price method
    boolean testTotalPrice(Tester t) {
        return t.checkInexact(this.rollingStone.totalPrice(), 2.55*12, .0001) 
        && t.checkInexact(this.houseOfCards.totalPrice(), 5.25*13, .0001)
        && t.checkInexact(this.serial.totalPrice(), 0.0, .0001)
        && t.checkInexact(this.time.totalPrice(), 3.0*12.0, .0001)
        && t.checkInexact(this.dopesick.totalPrice(), 6.0*8.0, .0001)
        && t.checkInexact(this.jre.totalPrice(), 0.0, 0.0001)
        && t.checkExpect(this.time.duration(), 300)
        && t.checkExpect(this.dopesick.duration(), 400)
        && t.checkExpect(this.jre.duration(), 50*2248)
        && t.checkExpect(this.time.format(), "Time, 3.0.")
        && t.checkExpect(this.time.sameEntertainment(this.time), true)
        && t.checkExpect(this.jre.sameEntertainment(this.jre), true)
        && t.checkExpect(this.dopesick.sameEntertainment(dopesick), true);
    }
    
}