import tester.*;

interface Picture {
  
  double getWidth();
  int countShapes();
  int comboDepth();
  Picture mirror();
  String pictureRecipe(int depth);
  
}

class Shape implements Picture {
  String kind;
  double size;
  Shape(String kind, double size) {
    this.kind = kind;
    this.size = size;
  }
  
  public double getWidth() {
    return this.size;
  }
  
  public int countShapes() {
    return 1;
  }
  
  public int comboDepth() {
    return 0;
  }
  
  public Picture mirror() {
    return this;
  }
  
  public String pictureRecipe(int depth) {
    return this.kind;
  }
}

class Combo implements Picture {
  String name;
  Operation op;
  Combo(String name, Operation op) {
    this.name = name;
    this.op = op;
  }
  
   public double getWidth() { return this.op.getWidth(); }
   
   public int countShapes() {
     return this.op.countShapes();
   }
   
   public int comboDepth() {
     return 1 + this.op.comboDepth();
   }
   
   public Picture mirror() {
     return new Combo(this.name, this.op.mirror());
   }
   
   public String pictureRecipe(int depth) {
     if (depth <= 0) { return this.name; }
     else { return this.op.pictureRecipe(depth - 1); }
   }
}

interface Operation {
  double getWidth();
  int countShapes();
  int comboDepth();
  Operation mirror();
  String pictureRecipe(int depth);
}

class Scale implements Operation {
  Picture picture;
  Scale(Picture picture) {
    this.picture = picture;
  }
  
  public double getWidth() {
    return this.picture.getWidth() * 2;
  }
  
  public int countShapes() {
    return this.picture.countShapes();
  }
  
  public int comboDepth() {
    return this.picture.comboDepth();
  }
  
  public Operation mirror() {
    return new Scale(this.picture.mirror());
  }
  
  public String pictureRecipe(int depth) {
    return "scale(" + this.picture.pictureRecipe(depth) + ")";
  }
  
}

class Beside implements Operation {
  Picture picture1;
  Picture picture2;
  Beside(Picture picture1, Picture picture2) {
    this.picture1 = picture1;
    this.picture2 = picture2;
  }
  
  public double getWidth() {
    return this.picture1.getWidth() + this.picture2.getWidth();
  }
  
  public int countShapes() {
    return this.picture1.countShapes() + this.picture2.countShapes();
  }
  
  public int comboDepth() {
    int pic1Depth = this.picture1.comboDepth();
    int pic2Depth = this.picture2.comboDepth();
    if (pic1Depth > pic2Depth) { return pic1Depth; }
    else { return pic2Depth; }
  }
  
  public Operation mirror() {
    return new Beside(this.picture2.mirror(), this.picture1.mirror());
  }
  
  public String pictureRecipe(int depth) {
    return "beside(" + this.picture1.pictureRecipe(depth) + ", " + this.picture2.pictureRecipe(depth) + ")";
  }
}

class Overlay implements Operation {
  Picture topPicture;
  Picture bottomPicture;
  Overlay(Picture topPicture, Picture bottomPicture) {
    this.topPicture = topPicture;
    this.bottomPicture = bottomPicture;
  }
  
  public double getWidth() {
    double topPicWidth = this.topPicture.getWidth();
    double bottomPicWidth = this.bottomPicture.getWidth();
    if (topPicWidth > bottomPicWidth) { return topPicWidth; }
    else { return bottomPicWidth; }
  }
  
  public int countShapes() {
    return this.topPicture.countShapes() + this.bottomPicture.countShapes();
  }
  
  public int comboDepth() {
    int topPicDepth = this.topPicture.comboDepth();
    int botPicDepth = this.bottomPicture.comboDepth();
    if (topPicDepth > botPicDepth) { return topPicDepth; }
    else { return botPicDepth; }
  }
  
  public Operation mirror() {
    return new Overlay(this.topPicture.mirror(), this.bottomPicture.mirror());
  }
  
  public String pictureRecipe(int depth) {
    return "overlay(" + this.topPicture.pictureRecipe(depth) + ", " + this.bottomPicture.pictureRecipe(depth) + ")";
  }
}

class ExamplesPicture {
  
  Shape circle = new Shape("circle", 20);
  Shape square = new Shape("square", 30);
  Combo bigCircle = new Combo("big circle", new Scale(circle));
  Combo squareOnCircle = new Combo("square on circle", new Overlay(square, bigCircle));
  Combo doubledSquareOnCircle = new Combo("doubled square on circle", new Beside(squareOnCircle, squareOnCircle));
  
  boolean testGetWidth(Tester t) {
    return 
        t.checkInexact(this.doubledSquareOnCircle.getWidth(), 80.0, 0.01) &&
        t.checkExpect(this.doubledSquareOnCircle.countShapes(), 4) &&
        t.checkExpect(this.doubledSquareOnCircle.comboDepth(), 3) &&
        t.checkExpect(this.doubledSquareOnCircle.pictureRecipe(2), "beside(overlay(square, big circle), overlay(square, big circle))") &&
        t.checkExpect(this.doubledSquareOnCircle.pictureRecipe(3), "beside(overlay(square, scale(circle)), overlay(square, scale(circle)))");
  }
}