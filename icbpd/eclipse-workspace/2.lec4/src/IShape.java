import tester.Tester;

// to represent a geometric shape
interface IShape {
  // to compute the area of this shape
  double area();
 
  // to compute the distance from this shape to the origin
  double distanceToOrigin();
   
  // to increase the size of this shape by the given increment
  IShape grow(int inc);
   
  // is the area of this shape bigger than
  // the area of the given shape?
  boolean isBiggerThan(IShape that);
  
  // is the given point within the shape
  boolean contains(Point point);
}

// to represent a point
interface Point {
  
  // distance from an origin
  double distanceToOrigin();
  
  //distance from another point
  double distTo(Point pt);
  
  // x-coord
  double x();
  
  // y-coord
  double y();
  
  // radii
  double radii();
  
  // theta
  double theta();
  
}

// represents a polar point
class PolarPt implements Point {
  double r;
  double theta;
  PolarPt(double r, double theta) {
    this.r = r;
    this.theta = theta;
  }
  
  public double distanceToOrigin() {
    return this.r;
  }
  
  public double distTo(Point pt) {
    return Math.sqrt(Math.pow(this.r, 2) + Math.pow(pt.radii(), 2) - 
        (2 * this.r * pt.radii() * Math.cos(pt.theta() - this.theta)));
  }
  
  public double x() {
    return this.r * Math.cos(this.theta);
  }
  
  public double y() {
    return this.r * Math.sin(this.theta);
  }
  
  public double radii() {
    return this.r;
  }
  
  public double theta() {
    return this.theta;
  }
}
 
// represents a cartesian point
class CartPt implements Point {
  int x;
  int y;
  CartPt(int x, int y) {
    this.x = x;
    this.y = y;
  }
  
  // compute the distance of a point from the origin
  public double distanceToOrigin() {
    return Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));
  }
  
  // computes distance from the given point
  public double distTo(Point pt) {
    return Math.sqrt(Math.pow((this.x - pt.x()), 2) + Math.pow((this.y - pt.y()), 2));    
  }
  
  public double x() {
    return this.x;
  }
  
  public double y() {
    return this.y;
  }
  
  // find radii using x and y coords
  public double radii() {
    return Math.sqrt(Math.pow(this.x, 2) + Math.pow(this.y, 2));
  }
  
  // find theta using x and y coords
  public double theta() {
    if (this.x == 0) {
      if (this.y == 0) {
        return 0;
      }
      else if (this.y > 0) {
        return 90;
      }
      else {
        return 270;
      }
    }
    else {
    double initial = Math.atan(this.y/this.x);
        if (this.x < 0) { // quadrant II and III
          return initial + 180;
        }
        else if (this.y <= 0) { // quadrant IV
          return initial + 360;
        }
        else {
          return initial;
        }
    }
  }
  
}

// represents a combination of IShapes
class Combo implements IShape {
  IShape shape1;
  IShape shape2;
  Combo(IShape shape1, IShape shape2) {
    this.shape1 = shape1;
    this.shape2 = shape2;
  }
  
  public double area() {
    return this.shape1.area() + this.shape2.area();
  }
  
  // returns the smaller distance to origin
  public double distanceToOrigin() {
    if (this.shape1.distanceToOrigin() <= this.shape2.distanceToOrigin()) {
      return this.shape1.distanceToOrigin();
    }
      return this.shape2.distanceToOrigin();
  }
  
  // to increase the size of this shape by the given increment
  public IShape grow(int inc) {
    return new Combo(this.shape1.grow(inc), this.shape2.grow(inc));
  }
   
  // is the area of this shape bigger than
  // the area of the given shape?
  public boolean isBiggerThan(IShape that) {
    return this.area() > that.area();
  }
  
  // is the given point within the shape
  public boolean contains(Point point) {
    return this.shape1.contains(point) || this.shape2.contains(point);
  }
}

// to represent a circle
class Circle implements IShape {
  Point center; // represents the center of the circle
  int radius;
  String color;
 
  Circle(Point center, int radius, String color) {
    this.center = center;
    this.radius = radius;
    this.color = color;
  }
 
  /* TEMPLATE
     FIELDS:
     ... this.center ...              -- Point
     ... this.radius ...              -- int
     ... this.color ...               -- String
     METHODS
     ... this.area() ...              -- double
  */
 
  // to compute the area of this shape
  public double area() {
    return Math.PI * this.radius * this.radius;
  }
  
  // In IShape
  // to compute the distance from this shape to the origin
  public double distanceToOrigin() {
    return this.center.distanceToOrigin() - this.radius;
  }
   
  // to increase the size of this shape by the given increment
  public IShape grow(int inc) { 
    return new Circle(this.center, this.radius + inc, this.color);
  }
   
  // is the area of this shape bigger than
  // the area of the given shape?
  public boolean isBiggerThan(IShape that) {
    return this.area() > that.area();
  }
  
  // is the given point within the shape
  public boolean contains(Point point) {
    return this.center.distTo(point) < this.radius;
  }
}
 
// to represent a square
class Square implements IShape {
  Point top_left; // represents the top-left corner of the square
  int size;
  String color;
 
  Square(Point top_left, int size, String color) {
    this.top_left = top_left;
    this.size = size;
    this.color = color;
  }
 
  /* TEMPLATE
     FIELDS:
     ... this.top_left ...        -- Point
     ... this.size ...            -- int
     ... this.color ...           -- String
     METHODS:
     ... this.area() ...                  -- double
  */
 
  // to compute the area of this shape
  public double area() {
    return this.size * this.size;
  }

  // In IShape
  // to compute the distance from this shape to the origin
  public double distanceToOrigin() {
    return this.top_left.distanceToOrigin();
   /* if (this.top_left.x() >= 0 && (this.top_left.y() - this.size) >= 0) { // in top right quadrant
      double ans = Math.sqrt((Math.pow(this.top_left.x(), 2) + Math.pow(this.top_left.y() - size, 2))) ;
      return ans;
    }
    else if (this.top_left.y() > 0 && (this.top_left.y() - this.size) <= 0) { // bisected by x-axis
      if (this.top_left.x() >= 0) { // positive x-axis
        double ans = this.top_left.x();
        return ans;
      }
      else if (this.top_left.x() < 0 && (this.top_left.x() + this.size) <= 0) { //negative x-axis
        double ans = Math.abs(this.top_left.x() + this.size); 
        return ans;
      }
    }
    else if (this.top_left.x() >= 0 && this.top_left.y() <= 0) { // in bottom right quadrant
      double ans =  Math.sqrt((Math.pow(this.top_left.x(),2) + Math.pow(this.top_left.y(),2)));
      return ans;
    }
    else if (this.top_left.x() < 0 && (this.top_left.x() + this.size) >= 0) { // bisected by y-axis
      if (this.top_left.y() <= 0) { // negative y-axis
        double ans = Math.abs(this.top_left.y()); 
        return ans;
      }
      else if (this.top_left.y() > 0 && (this.top_left.y() - this.size) >= 0) { // positive y-axis
        double ans = (this.top_left.y() - this.size);
        return ans;
      }
    }
    else if (this.top_left.y() <= 0 && this.top_left.x() <= 0) { // in bottom left quadrant
      double ans = Math.sqrt((Math.pow(this.top_left.y(), 2) + Math.pow(this.top_left.x() + this.size, 2)));
      return ans;
    }
    else if (this.top_left.y() >= 0 && (this.top_left.y() - this.size) > 0 && this.top_left.x() <= 0 && 
        (this.top_left.x() + this.size) < 0) {
      // in top left quadrant
      double ans = Math.sqrt((Math.pow(this.top_left.x() + this.size, 2) + Math.pow(this.top_left.y() - this.size, 2)));
      return ans;
    }
    return 0.0; */
  } 
  
 
  // to increase the size of this shape by the given increment
  public IShape grow(int inc) {
    return new Square(this.top_left, this.size + inc, this.color);
    }
 
  // is the area of this shape bigger than
  // the area of the given shape?
  public boolean isBiggerThan(IShape that) {
    return this.area() > that.area();
    }
  
  // is the point within the square
  public boolean contains(Point point) {
    return (point.x() >= this.top_left.x() && point.x() <= (this.top_left.x() + this.size) &&
        point.y() <= this.top_left.y() && point.y() >= (this.top_left.y() - this.size));
  }
}

 
class ExamplesShapes {
  ExamplesShapes() {}
  
  CartPt pt1 = new CartPt(0, 0);
  CartPt pt2 = new CartPt(3, 4);
  CartPt pt3 = new CartPt(7, 1);
 
  IShape c1 = new Circle(new CartPt(50, 50), 10, "red");
  IShape c2 = new Circle(new CartPt(50, 50), 30, "red");
  IShape c3 = new Circle(new CartPt(30, 100), 30, "blue");
 
  IShape s1 = new Square(new CartPt(50, 50), 30, "red");
  IShape s2 = new Square(new CartPt(50, 50), 50, "red");
  IShape s3 = new Square(new CartPt(20, 40), 10, "green");
 
  // test the method distanceToOrigin in the class CartPt
  boolean testDistanceToOrigin(Tester t) {
    return
    t.checkInexact(this.pt1.distanceToOrigin(), 0.0, 0.001) &&
    t.checkInexact(this.pt2.distanceToOrigin(), 5.0, 0.001);
  }
 
  // test the method distTo in the class CartPt
  boolean testDistTo(Tester t) {
    return
    t.checkInexact(this.pt1.distTo(this.pt2), 5.0, 0.001) &&
    t.checkInexact(this.pt2.distTo(this.pt3), 5.0, 0.001);
  }
 
  // test the method area in the class Circle
  boolean testCircleArea(Tester t) {
    return
    t.checkInexact(this.c1.area(), 314.15, 0.01);
  }
 
  // test the method area in the class Square
  boolean testSquareArea(Tester t) {
    return
    t.checkInexact(this.s1.area(), 900.0, 0.01);
  }
 
  // test the method distanceToOrigin in the class Circle
  boolean testCircleDistanceToOrigin(Tester t) {
    return
    t.checkInexact(this.c1.distanceToOrigin(), 60.71, 0.01) &&
    t.checkInexact(this.c3.distanceToOrigin(), 74.40, 0.01);
  }
 
  // test the method distanceToOrigin in the class Square
  boolean testSquareDistanceToOrigin(Tester t) {
    return
    t.checkInexact(this.s1.distanceToOrigin(), 70.71, 0.01) &&
    t.checkInexact(this.s3.distanceToOrigin(), 44.72, 0.01);
  }
 
  // test the method grow in the class Circle
  boolean testCircleGrow(Tester t) {
    return
    t.checkExpect(this.c1.grow(20), this.c2);
  }
 
  // test the method grow in the class Square
  boolean testSquareGrow(Tester t) {
    return
    t.checkExpect(this.s1.grow(20), this.s2);
  }
 
  // test the method isBiggerThan in the class Circle
  boolean testCircleIsBiggerThan(Tester t) {
    return
    t.checkExpect(this.c1.isBiggerThan(this.c2), false) &&
    t.checkExpect(this.c2.isBiggerThan(this.c1), true) &&
    t.checkExpect(this.c1.isBiggerThan(this.s1), false) &&
    t.checkExpect(this.c1.isBiggerThan(this.s3), true);
  }
 
  // test the method isBiggerThan in the class Square
  boolean testSquareIsBiggerThan(Tester t) {
    return
    t.checkExpect(this.s1.isBiggerThan(this.s2), false) &&
    t.checkExpect(this.s2.isBiggerThan(this.s1), true) &&
    t.checkExpect(this.s1.isBiggerThan(this.c1), true) &&
    t.checkExpect(this.s3.isBiggerThan(this.c1), false);
  }
 
   // test the method contains in the class Circle
  boolean testCircleContains(Tester t) {
    return
    t.checkExpect(this.c1.contains(new CartPt(100, 100)), false) &&
    t.checkExpect(this.c2.contains(new CartPt(40, 60)), true);
  }
 
 
  // test the method contains in the class Square
  boolean testSquareContains(Tester t) {
    return
    t.checkExpect(this.s1.contains(new CartPt(100, 100)), false) &&
    t.checkExpect(this.s2.contains(new CartPt(55, 60)), true);
  }
}