import tester.*;                // The tester library
import javalib.worldimages.*;   // images, like RectangleImage or OverlayImages
import javalib.funworld.*;      // the abstract World class and the big-bang library
import java.awt.Color;          // general colors (as triples of red,green,blue values)
                                // and predefined colors (Color.RED, Color.GRAY, etc.)
import javalib.worldcanvas.*;

interface ITree { 
  WorldImage draw();
  WorldImage draw(double otherTheta);
  boolean isDrooping();
  ITree combine(int leftLength, int rightLength, double leftTheta, double rightTheta, ITree otherTree);
  ITree rotate(double otherTheta);
  double getWidth();
  double rightWidth(double otherTheta);
  double leftWidth(double otherTheta);
}

class PolarPt {
  double magnitude;
  double theta;
  PolarPt(double magnitude, double theta) {
    this.magnitude = magnitude;
    this.theta = theta;
  }
  
  Double x() {
    return this.magnitude * Math.cos(Math.toRadians(this.theta));
  }
  
  Double y() {
    return this.magnitude * Math.sin(Math.toRadians(this.theta));
  }
  
  Posn toPosn() {
    return new Posn(this.x().intValue(), this.y().intValue());
  }
  
}

class Leaf implements ITree {
  int size; // represents the radius of the leaf
  Color color; // the color to draw it
  Leaf(int size, Color color) {
    this.size = size;
    this.color = color;
  }
  
  public WorldImage draw() {
    return new CircleImage(this.size, "solid", this.color);
  }
  
  public WorldImage draw(double otherTheta) {
    PolarPt new_pinhole = new PolarPt(this.size, -otherTheta - 180);
    return new CircleImage(this.size, "solid", this.color).movePinhole(new_pinhole.x(),new_pinhole.y());
  }
  
  public boolean isDrooping() {
    return false;
  }
  
  public ITree combine(int leftLength, int rightLength, double leftTheta, double rightTheta, ITree otherTree) {
    return new Branch(leftLength, rightLength, leftTheta, rightTheta, this, otherTree.rotate(rightTheta));
  }
  
  public ITree rotate(double otherTheta) {
    return this;
  }
  
  public double getWidth() {
    return 2 * this.size;
  }
  
  public double leftWidth(double otherTheta) {
    return this.size * Math.abs(Math.cos(Math.toRadians(180 - otherTheta))) + this.size;
  }
  
  public double rightWidth(double otherTheta) {
    return this.size * Math.abs(Math.cos(Math.toRadians(otherTheta))) + this.size;
  }
}
 
class Stem implements ITree {
  // How long this stick is
  int length;
  // The angle (in degrees) of this stem, relative to the +x axis
  double theta;
  // The rest of the tree
  ITree tree;
  Stem(int length, double theta, ITree tree) {
    this.length = length;
    this.theta = theta;
    this.tree = tree;
  }
  
  public WorldImage stem_image() {
    return new LineImage(this.end_pt().toPosn(), Color.gray);
  }
  
  public WorldImage draw() {
    return new OverlayImage(this.tree.draw(this.theta), this.stem_image().movePinhole(this.stem_pinhole().x(),this.stem_pinhole().y()))
        .movePinhole(this.origin().x(),this.origin().y());
  }
  
  public WorldImage draw(double otherTheta) {
    return this.draw();
  }
  
  public PolarPt end_pt() {
    return new PolarPt(this.length, -this.theta);
  }
  
  public PolarPt stem_pinhole() {
    return new PolarPt(this.length / 2, -this.theta);
  }
  
  public PolarPt origin() {
    return new PolarPt(-this.length, -this.theta);
  }
  
  public boolean isDrooping() {
    return (this.theta > 180 && this.theta < 360) || this.isDroopingHelp(1) || this.tree.isDrooping();
  }
  
  boolean isDroopingHelp(int mult) {
    double currentTheta = this.theta - (360 * mult);
    if (currentTheta < 0) {
      return false;
    }
    else if (currentTheta > 360) {
      return this.isDroopingHelp(mult + 1);
    }
    else { return currentTheta > 180 && currentTheta < 360; } 
  }
  
  public ITree combine(int leftLength, int rightLength, double leftTheta, double rightTheta, ITree otherTree) {
    return new Branch(leftLength, rightLength, leftTheta, rightTheta, this.rotate(leftTheta), otherTree.rotate(rightTheta));
  }
  
  public ITree rotate(double otherTheta) {
    double rotate_amt = 90 - otherTheta;
    return new Stem(this.length, this.theta - rotate_amt, this.tree.rotate(otherTheta));
  }
  
  public double getWidth() {
    double stemWidth = this.end_pt().x();
    if (stemWidth > 0) {
      return Math.abs(stemWidth) + this.tree.rightWidth(this.theta);
    }
    else if (stemWidth < 0) {
      return Math.abs(stemWidth) + this.tree.leftWidth(this.theta);
    }
    else { return this.tree.getWidth(); }
  }
  
  public double rightWidth(double otherTheta) {
    return new Stem(this.length, this.theta + otherTheta - 90, this.tree).getWidth();
  }
  
  public double leftWidth(double otherTheta) {
    return this.rightWidth(otherTheta);
  }
}
 
class Branch implements ITree {
  // How long the left and right branches are
  int leftLength;
  int rightLength;
  // The angle (in degrees) of the two branches, relative to the +x axis,
  double leftTheta;
  double rightTheta;
  // The remaining parts of the tree
  ITree left;
  ITree right;
  Branch(int leftLength, int rightLength, double leftTheta, double rightTheta, ITree left, ITree right) {
    this.leftLength = leftLength;
    this.rightLength = rightLength;
    this.leftTheta = leftTheta;
    this.rightTheta = rightTheta;
    this.left = left;
    this.right = right;
  }
  
  public WorldImage draw() {
    return new OverlayImage(this.left_stem().draw(), 
        this.right_stem().draw());
  }
  
  public WorldImage draw(double other_theta) {
    return this.draw();
  }
  
  public Stem left_stem() {
    return new Stem(this.leftLength, this.leftTheta, this.left);
  }
  
  public Stem right_stem() {
    return new Stem(this.rightLength, this.rightTheta, this.right);
  }
  
  public boolean isDrooping() {
    return this.left_stem().isDrooping() || this.right_stem().isDrooping();
  }
  
  public ITree combine(int leftLength, int rightLength, double leftTheta, double rightTheta, ITree otherTree) {
    return new Branch(leftLength, rightLength, leftTheta, rightTheta, this.rotate(leftTheta), otherTree.rotate(rightTheta));
  }
  
  public ITree rotate(double otherTheta) {
    double rotate_amt = otherTheta - 90;
    return new Branch(this.leftLength, this.rightLength, this.leftTheta + rotate_amt, this.rightTheta + rotate_amt, 
        this.left.rotate(otherTheta), this.right.rotate(otherTheta));
  }
  
  public double getWidth() {
    return this.right_stem().getWidth() + this.left_stem().getWidth();
  }
  
  public double rightWidth(double otherTheta) {
    return this.right_stem().getWidth();
  }
  
  public double leftWidth(double otherTheta) {
    return this.left_stem().getWidth();
  }
  
}

class ExamplesTree {
  
  Leaf leaf1 = new Leaf(10, Color.red);
  Stem tree0 = new Stem(20,135, new Leaf(10, Color.red));
  Branch tree1 = new Branch(30, 30, 135, 40, new Leaf(10, Color.RED), new Leaf(15, Color.BLUE));
  Branch tree2 = new Branch(30, 30, 115, 65, new Leaf(15, Color.GREEN), new Leaf(8, Color.ORANGE));
  Stem tree3 = new Stem(40, 90, tree1);
  Stem tree4 = new Stem(50, 90, tree2);
  Branch tree5 = new Branch(30, 30, 190, 65, new Leaf(15, Color.GREEN), new Leaf(8, Color.ORANGE));
  Branch tree6 = new Branch(40, 50, 150, 30, tree1, tree2);
  ITree tree7 = tree1.combine(40, 50, 150, 30, tree2);
  
  boolean testPoint(Tester t) {
    return
        t.checkInexact(Math.cos(Math.toRadians(90)), 0.0, 0.001);
  }
  
/*  boolean testDrawTree(Tester t) {
    WorldCanvas c = new WorldCanvas(500, 500);
    WorldScene s = new WorldScene(500, 500);
    return c.drawScene(s.placeImageXY(new VisiblePinholeImage(this.tree0.draw()), 250, 250))
        && c.show();
  } */ 
  
  boolean testIsDrooping(Tester t) {
    return 
        t.checkExpect(this.tree3.isDrooping(), false) &&
        t.checkExpect(this.tree5.isDrooping(), true) &&
        t.checkExpect(this.tree0.getWidth(), 27.0);
  }
      
}