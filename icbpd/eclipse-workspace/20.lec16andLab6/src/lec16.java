interface IShape { 
  double accept(IShapeVisitor<Double> visitor);
}
class Circle implements IShape {
  int x, y;
  int radius;
  String color;
  Circle(int x, int y, int r, String color) {
    this.x = x;
    this.y = y;
    this.radius = r;
    this.color = color;
  }
  
  public double accept(IShapeVisitor<Double> visitor) {
    return visitor.visitCircle(this);
  }
}

class Rect implements IShape {
  int x, y, w, h;
  String color;
  Rect(int x, int y, int w, int h, String color) {
    this.x = x;
    this.y = y;
    this.w = w;
    this.h = h;
    this.color = color;
  }
  
  public double accept(IShapeVisitor<Double> visitor) {
    return visitor.visitRect(this);
  }
}

class Square implements IShape {
  int x, y, size;
  String color;
  Square(int x, int y, int size, String color) {
    this.x = x;
    this.y = y;
    this.size = size;
    this.color = color;
  }
  public double accept(IShapeVisitor<Double> visitor) {
    return visitor.visitSquare(this);
  }
}

interface IShapeVisitor<T> extends IFunc<IShape, T> {
  T visitCircle(Circle circle);
  T visitRect(Rect rect);
  T visitSquare(Square square);
}

//Implements a function taking a Shape and returning a Double,
//that computes the area of the given shape
class ShapeArea implements IShapeVisitor<Double> {
  public Double visitCircle(Circle circle) {
    return Math.PI * circle.radius * circle.radius;
  }
  public Double visitRect(Rect rect) {
    return rect.w * rect.h * 1.0;
  }
  public Double visitSquare(Square square) {
    return square.size * square.size * 1.0;
  }
  public Double apply(IShape shape) {
    return shape.accept(this);
  }
}

class ShapePerimeter implements IShapeVisitor<Double> {
  public Double visitCircle(Circle circle) {
    return 2 * Math.PI * circle.radius;
  }
  public Double visitRect(Rect rect) {
    return (2.0 * rect.w) + (2.0 * rect.h);
  }
  public Double visitSquare(Square square) {
    return 4.0 * square.size;
  }
  public Double apply(IShape shape) {
    return shape.accept(this);
  }
}
