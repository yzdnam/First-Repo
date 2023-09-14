import javalib.worldimages.*;   // images, like RectangleImage or OverlayImages
import javalib.funworld.*;      // the abstract World class and the big-bang library

class MyPosn {
  double x;
  double y;
  // standard constructor
  MyPosn(double x, double y) {
    this.x = x;
    this.y = y;
  }
  
  MyPosn add(MyPosn that) {
    return new MyPosn(that.addToX(this.x), that.addToY(this.y));
  }
  
  double addToX(double otherX) {
    return this.x + otherX;
  }
  
  double addToY(double otherY) {
    return this.y + otherY;
  }
  
  boolean isOffscreen(int screenX, int screenY) {
    return this.x < 0 || this.x > screenX || this.y < 0 || this.y > screenY;
  }
  
  double positionX() {
    return this.x;
  }
  
  double positionY() {
    return this.y;
  }
  
}

interface IGamePiece extends Consts {
  double positionX();
  
  double positionY();
  
  IGamePiece move();
  
  boolean isOffscreen(int screenX, int screenY);
  
  boolean hit(IGamePiece other);
  
  public boolean survived(IList<IGamePiece> others);
  
  WorldImage draw();
  
  WorldScene place(WorldScene scene);
  
  IList<IGamePiece> explode();
}

abstract class AGamePiece implements IGamePiece {
  MyPosn position; // in pixels
  MyPosn velocity; // in pixels/tick
  
/*  AGamePiece(MyPosn position, MyPosn velocity) {
    this.position = position;
    this.velocity = velocity;
  } */
  
  public double positionX() {
    return this.position.positionX();
  }
  
  public double positionY() {
    return this.position.positionY();
  }
  
  public abstract IGamePiece move();
  
  public boolean isOffscreen(int screenX, int screenY) {
    return this.position.isOffscreen(screenX, screenY);
  }
  
  public boolean hit(IGamePiece other) {
    return Math.hypot(this.positionX() - other.positionX(), this.positionY() - other.positionY()) <= SHIP_RADIUS;
  }
  
  public boolean survived(IList<IGamePiece> others) {
    return !others.orMap(new AnyHitOne(), this);
  }
  
  public abstract WorldImage draw();
  
  public WorldScene place(WorldScene scene) {
    return scene.placeImageXY(this.draw(), (int)this.positionX(), (int)this.positionY());
  }
  
  public abstract IList<IGamePiece> explode();
}

class Ship extends AGamePiece {
  Ship(MyPosn position, MyPosn velocity) {
    this.position = position;
    this.velocity = velocity;
  }
  
  Ship(MyPosn position) {
    if (position.x == SCREEN_WIDTH) {
      this.position = position;
      this.velocity = new MyPosn(-SHIP_SPEED, 0);
    }
    else {
      this.position = position;
      this.velocity = new MyPosn(SHIP_SPEED, 0);
    }
  }
  
  public IGamePiece move() {
    return new Ship(this.position.add(this.velocity), this.velocity);
  }
  
  public WorldImage draw() {
    return new CircleImage(SHIP_RADIUS, OutlineMode.SOLID, SHIP_COLOR);
  }
  
  public IList<IGamePiece> explode() {
    return new MtList<IGamePiece>();
  }

}

class Bullet extends AGamePiece {
  int explosion;
  Bullet(MyPosn position, MyPosn velocity, int explosion) {
    this.position = position;
    this.velocity = velocity;
    this.explosion = explosion;
  }
  
  public IGamePiece move() {
    return new Bullet(this.position.add(this.velocity), this.velocity, this.explosion);
  }
  
  public WorldImage draw() {
    if ((INC_BULLET_RADIUS * explosion) > MAX_BULLET_RADIUS) {
      return new CircleImage(MAX_BULLET_RADIUS, OutlineMode.SOLID, BULLET_COLOR);
    }
    else return new CircleImage(INC_BULLET_RADIUS * explosion, OutlineMode.SOLID, BULLET_COLOR);
  }
  
  public IList<IGamePiece> explode() {
    return this.createNewBullets(this.explosion, new MtList<IGamePiece>(), 360 / (this.explosion + 1));
  }
  
  private IList<IGamePiece> createNewBullets(int bulletsToCreate, IList<IGamePiece> bulletsCreated, double baseDirection) {
    double directionInDegrees = bulletsToCreate * baseDirection;
    if (bulletsToCreate < 0) {
      return bulletsCreated;
    }
    else 
      return 
          this.createNewBullets(bulletsToCreate - 1, 
              new ConsList<IGamePiece>(new Bullet(this.position, 
                                                  new MyPosn(BULLET_SPEED * Math.cos(Math.toRadians(directionInDegrees)), 
                                                             BULLET_SPEED * Math.sin(Math.toRadians(directionInDegrees))), 
                                                      this.explosion + 1), bulletsCreated), 
              baseDirection);
  }
  
}

class ShipSpawn implements Consts {
  
  IList<IGamePiece> newSpawn(IList<IGamePiece> ships, int ticks, int shipsToSpawn) {
    if (Integer.remainderUnsigned(ticks, SHIP_SPAWN_INTERVAL) == 0) {
      return this.addShips(ships, shipsToSpawn);
    }
    else return ships;
  }
  
  IList<IGamePiece> addShips(IList<IGamePiece> ships, int shipsToSpawn) {
    if (shipsToSpawn == 0) {
      return ships;
    }
    else return this.addShips(new ConsList<IGamePiece>
    (new Ship(new MyPosn(rand.nextInt(2) * SCREEN_WIDTH, rand.nextInt(SCREEN_HEIGHT * 5 / 7) + (SCREEN_HEIGHT / 7))), ships), shipsToSpawn - 1);
  }
}

class BulletShoot implements Consts {
  
  IList<IGamePiece> addBullet(IList<IGamePiece> bullets) {
    return 
        new ConsList<IGamePiece>(new Bullet(new MyPosn(BULLET_SPAWN_X, BULLET_SPAWN_Y), 
            new MyPosn(0, -BULLET_SPEED), 1), bullets);
  }
}

