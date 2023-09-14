import tester.*;
import javalib.worldimages.*;   // images, like RectangleImage or OverlayImages
import javalib.funworld.*;      // the abstract World class and the big-bang library
import java.awt.Color;          // general colors (as triples of red,green,blue values)
import java.util.Random;

class ExampleGame implements Consts {
  boolean testBigBang(Tester t) {
    Game g = new Game(100);
    int worldWidth = SCREEN_WIDTH;
    int worldHeight = SCREEN_HEIGHT;
    double tickRate = TICK_RATE;
    return g.bigBang(worldWidth, worldHeight, tickRate);
  } 
}

interface Consts {
  
  Random rand = new Random();
  
  int SCREEN_WIDTH = 500;
  int SCREEN_HEIGHT = 300;
  Double TICK_RATE = 1.0 / 28.0; // seconds per frame
  
  int INITIAL_BULLET_RADIUS = 2;
  int INC_BULLET_RADIUS = 2;
  int MAX_BULLET_RADIUS = 10;
  Color BULLET_COLOR = Color.pink;
  int BULLET_SPEED = 8;
  int BULLET_SPAWN_X = SCREEN_WIDTH / 2;
  int BULLET_SPAWN_Y = SCREEN_HEIGHT * 4 / 5;
  
  int SHIPS_TO_SPAWN = rand.nextInt(3) + 1;
  int SHIP_RADIUS = SCREEN_HEIGHT / 30;
  Color SHIP_COLOR = Color.cyan;
  int SHIP_SPEED = BULLET_SPEED / 2;
  int SHIP_SPAWN_Y = rand.nextInt(SCREEN_HEIGHT * 5 / 7) + (SCREEN_HEIGHT / 7);
  int SHIP_SPAWN_X = rand.nextInt(2) * SCREEN_WIDTH;
  int SHIP_SPAWN_INTERVAL = 28; // every 28 frames or 1 second
  
  Color FONT_COLOR = Color.black;
  int FONT_SIZE = 13;
  
  int SCOREBOARD_X = SCREEN_WIDTH / 2;
  int SCOREBOARD_Y = SCREEN_HEIGHT * 19 / 20;
  
}

class Game extends World implements Consts {
  IList<IGamePiece> bulletsOnScreen;
  IList<IGamePiece> shipsOnScreen;
  int bulletsLeft;
  int shipsDestroyed;
  WorldScene scene;
  int ticksElapsed;
  
  Game(IList<IGamePiece> bulletsOnScreen, IList<IGamePiece> shipsOnScreen, int bulletsLeft, int shipsDestroyed, WorldScene scene, int ticksElapsed) {
    this.bulletsOnScreen = bulletsOnScreen;
    this.shipsOnScreen = shipsOnScreen;
    this.bulletsLeft = bulletsLeft;
    this.shipsDestroyed = shipsDestroyed;
    this.scene = scene;
    this.ticksElapsed = ticksElapsed;
  }
  
  Game(int bulletsLeft) {
    this(new MtList<IGamePiece>(), new MtList<IGamePiece>(), bulletsLeft, 0, new WorldScene(SCREEN_WIDTH, SCREEN_HEIGHT), 0);
  }
  
  TextImage scoreBoard = new TextImage("Bullets Left: " + bulletsLeft + "  Ships Destroyed: " + shipsDestroyed, 
      FONT_SIZE, FontStyle.BOLD, FONT_COLOR);
  
  public WorldScene makeScene() {
    return this.shipsOnScreen.foldr(new PlaceAll(), 
        this.bulletsOnScreen.foldr(new PlaceAll(), this.scene))
        .placeImageXY(new TextImage("Bullets Left: " + bulletsLeft + "  Ships Destroyed: " + shipsDestroyed, 
            FONT_SIZE, FontStyle.BOLD, FONT_COLOR), SCOREBOARD_X, SCOREBOARD_Y);
  }
  
  public WorldScene finalScene() {
    return this.scene.placeImageXY(new TextImage("GAME OVER", 50, FontStyle.BOLD, Color.RED), this.scene.width / 2, 
        this.scene.height / 2).placeImageXY(new TextImage("Bullets Left: " + bulletsLeft + "  Ships Destroyed: " + shipsDestroyed, 
            FONT_SIZE, FontStyle.BOLD, FONT_COLOR), SCOREBOARD_X, SCOREBOARD_Y);
  }
  
  public WorldEnd worldEnds() {
    if (this.bulletsLeft <= 0 && this.bulletsOnScreen.length() == 0) {
      return new WorldEnd(true, this.finalScene());
    } else {
      return new WorldEnd(false, this.makeScene());
    }
  } 

  public Game onKeyEvent(String key) {
    if (key.equals("shift") && this.bulletsLeft > 0) {
    return new Game(new BulletShoot().addBullet(this.bulletsOnScreen), this.shipsOnScreen, this.bulletsLeft - 1, this.shipsDestroyed, this.scene, 
        this.ticksElapsed);
    }
    else return this;
  }
  
  public Game onTick() {
    return this.removeOffScreen().detectHits().spawnMoveTick();
  }
  
  private Game detectHits() {
    IList<IGamePiece> shipsNotHit = this.shipsOnScreen.filter(new NotHitBy(bulletsOnScreen));
    return new Game(new FlatBulletList().flattenList(this.bulletsOnScreen.map(new HitAndExplode(shipsOnScreen))), 
        shipsNotHit,
        this.bulletsLeft, this.shipsDestroyed + (this.shipsOnScreen.length() - shipsNotHit.length()), this.scene, this.ticksElapsed);
  }
  
  private Game removeOffScreen() {
    return new Game(this.bulletsOnScreen.filter(new NotOffScreen()),
        this.shipsOnScreen.filter(new NotOffScreen()),
        this.bulletsLeft, this.shipsDestroyed, this.scene, this.ticksElapsed);
  }
  
  private Game spawnMoveTick() {
    return new Game(this.bulletsOnScreen.map(new Move()), 
        new ShipSpawn().newSpawn(this.shipsOnScreen, this.ticksElapsed, rand.nextInt(3) + 1).map(new Move()),
        this.bulletsLeft, this.shipsDestroyed, this.scene, this.ticksElapsed + 1);
  }

}

class FlatBulletList {

  IList<IGamePiece> flattenList(IList<IList<IGamePiece>> bullets) {
    return bullets.foldr(new Flatten(), new MtList<IGamePiece>());
  }
}

class Flatten implements IFunc2<IList<IGamePiece>, IList<IGamePiece>, IList<IGamePiece>> {
  public IList<IGamePiece> apply(IList<IGamePiece> firstList, IList<IGamePiece> base) { 
    return firstList.append(base); 
    }
}

class NotOffScreen implements IPred<IGamePiece>, Consts {
  public boolean apply(IGamePiece p) { return !p.isOffscreen(SCREEN_WIDTH, SCREEN_HEIGHT); }
}

class PlaceAll implements IFunc2<IGamePiece, WorldScene, WorldScene> {
  public WorldScene apply(IGamePiece p , WorldScene s) { return p.place(s); } 
}

class AnyHitOne implements IPred2<IGamePiece> {
  public boolean apply(IGamePiece p, IGamePiece base) { return p.hit(base); }
}

class NotHitBy implements IPred<IGamePiece> {
  IList<IGamePiece> others;
  NotHitBy(IList<IGamePiece> others) {
    this.others = others;
  }
  public boolean apply(IGamePiece p) { return p.survived(others); }
}

class HitAndExplode implements IFunc<IGamePiece, IList<IGamePiece>> {
  IList<IGamePiece> others;
  HitAndExplode(IList<IGamePiece> others) {
    this.others = others;
  }
  public IList<IGamePiece> apply(IGamePiece p) { 
    if (!p.survived(others)) {
      return p.explode();
    }
    else return new ConsList<IGamePiece>(p, new MtList<IGamePiece>());
  }
}

class Move implements IFunc<IGamePiece, IGamePiece> {
  public IGamePiece apply(IGamePiece p) { return p.move(); }
}
