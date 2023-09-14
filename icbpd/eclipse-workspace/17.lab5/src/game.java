import tester.*;                // The tester library
import javalib.worldimages.*;   // images, like RectangleImage or OverlayImages
import javalib.funworld.*;      // the abstract World class and the big-bang library
import java.awt.Color;          // general colors (as triples of red,green,blue values)
                                // and predefined colors (Color.RED, Color.GRAY, etc.)
import javalib.worldcanvas.*;
import java.util.Random;

class ExampleImages {
  boolean testBigBang(Tester t) {
    Game g = new Game(5);
    int worldWidth = 500;
    int worldHeight = 500;
    double tickRate = 1.0 / 5.0;
    return g.bigBang(worldWidth, worldHeight, tickRate);
  } 
}

class MyPosn extends Posn {
  
  // standard constructor
  MyPosn(int x, int y) {
    super(x, y);
  }
 
  // constructor to convert from a Posn to a MyPosn
  MyPosn(Posn p) {
    this(p.x, p.y);
  }
  
  MyPosn add(MyPosn that) {
    return new MyPosn(that.addToX(this.x), that.addToY(this.y));
  }
  
  int addToX(int otherX) {
    return this.x + otherX;
  }
  
  int addToY(int otherY) {
    return this.y + otherY;
  }
  
  boolean isOffscreen(int screenX, int screenY) {
    return this.x < 0 || this.x > screenX || this.y < 0 || this.y > screenY;
  }
  
  int positionX() {
    return this.x;
  }
  
  int positionY() {
    return this.y;
  }
  
}

interface IShape {
  int DEFAULT_RADIUS = 10;
}

class Circle implements IShape {
  MyPosn position; // in pixels
  MyPosn velocity; // in pixels/tick
  int radius;
  Circle(MyPosn position, MyPosn velocity, int radius) {
    this.position = position;
    this.velocity = velocity;
    this.radius = radius;
  }
  Circle(MyPosn position, MyPosn velocity) {
    this(position, velocity, DEFAULT_RADIUS);
  }
  
  int positionX() {
    return this.position.positionX();
  }
  
  int positionY() {
    return this.position.positionY();
  }
  
  Circle move() {
    return new Circle(this.position.add(this.velocity), this.velocity, this.radius);
  }
  
  boolean isOffscreen(int screenX, int screenY) {
    return this.position.isOffscreen(screenX, screenY);
  }
  
  WorldImage draw() {
    return new CircleImage(this.radius, OutlineMode.SOLID, Color.BLUE);
  }
  
  WorldScene place(WorldScene scene) {
    return scene.placeImageXY(this.draw(), this.positionX(), this.positionY());
  }
}

interface ICircleFunc {
  Circle apply(Circle c);
}
class MoveCircle implements ICircleFunc {
  public Circle apply(Circle c) { return c.move(); }
}

interface ILoCircle {
  ILoCircle map(ICircleFunc fun);
  ILoCircle moveAll();
  boolean isOffscreen(int screenX, int screenY);
  ILoCircle removeOffscreen(int screenX, int screenY);
  WorldScene placeAll(WorldScene scene);
  int len();
}

class ConsC implements ILoCircle {
  Circle first;
  ILoCircle rest;
  ConsC(Circle first, ILoCircle rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public WorldScene placeAll(WorldScene scene) {
    return this.rest.placeAll(first.place(scene));
  }
  
  public ILoCircle removeOffscreen(int screenX, int screenY) {
    if (this.first.isOffscreen(screenX, screenY)) {
      return this.rest.removeOffscreen(screenX, screenY);
    }
    else 
      return new ConsC(this.first, this.rest.removeOffscreen(screenX, screenY));
  }
  
  public boolean isOffscreen(int screenX, int screenY) {
    return this.first.isOffscreen(screenX, screenY) || this.rest.isOffscreen(screenX, screenY);
  }
  
  public ILoCircle moveAll() {
    return this.map(new MoveCircle());
  }
  
  public ILoCircle map(ICircleFunc fun) {
    return new ConsC(fun.apply(this.first), this.rest.map(fun));
  }
  
  public int len() {
    return 1 + this.rest.len();
  }
}

class MtLoC implements ILoCircle {
  MtLoC() {}
  
  public WorldScene placeAll(WorldScene scene) {
    return scene;
  }
  
  public ILoCircle removeOffscreen(int screenX, int screenY) {
    return this;
  }
  
  public boolean isOffscreen(int screenX, int screenY) {
    return false;
  }
  
  public ILoCircle moveAll() {
    return this;
  }
  
  public ILoCircle map(ICircleFunc fun) {
    return this;
  }
  
  public int len() {
    return 0;
  }
}

class Game extends World {
  ILoCircle circlesOnScreen;
  int circlesLeft;
  WorldScene scene;
  Game(ILoCircle circlesOnScreen, int circlesLeft, WorldScene scene) {
    this.circlesOnScreen = circlesOnScreen;
    this.circlesLeft = circlesLeft;
    this.scene = scene;
  }
  Game(int circlesLeft) {
    this(new MtLoC(), circlesLeft, new WorldScene(500, 500));
  }
  
  Random rand = new Random();
  
  public WorldScene makeScene() {
    return this.circlesOnScreen.placeAll(this.scene);
  }
  
  public WorldScene finalScene() {
    return this.scene.placeImageXY(new TextImage("GAME OVER", 50, FontStyle.BOLD, Color.RED), this.scene.width / 2, 
        this.scene.height / 2);
  }
  
  public WorldEnd worldEnds() {
    if (this.circlesLeft <= 0) {
      return new WorldEnd(true, this.finalScene());
    } else {
      return new WorldEnd(false, this.makeScene());
    }
  }
  
  public Game onMouseClicked(Posn pos) {
    int maxVelocity = 100;
    return new Game(new ConsC(new Circle(new MyPosn(pos), 
                                         new MyPosn(rand.nextInt(maxVelocity) - rand.nextInt(maxVelocity), 
                                                    rand.nextInt(maxVelocity) - rand.nextInt(maxVelocity))),
                    this.circlesOnScreen), this.circlesLeft, this.scene);
  }
  
  public Game onTick() {
    ILoCircle newCircles = this.circlesOnScreen.removeOffscreen(this.scene.width, this.scene.height).moveAll();
    return new Game(newCircles, this.circlesLeft - this.circlesOnScreen.len() + newCircles.len(), this.scene);
  }
}