import tester.*;

interface IGamePiece {
  int getValue();
  IGamePiece merge(IGamePiece other_gp);
  boolean isValid();
}

abstract class AGamePiece implements IGamePiece {
  AGamePiece() {}
  
  public abstract int getValue();
  
  public IGamePiece merge(IGamePiece other_gp) {
    return new MergeTile(this, other_gp);
  }
  
  public abstract boolean isValid();
  
}

class BaseTile extends AGamePiece {
  int value;
  BaseTile(int value) {
    this.value = value;
  }
  
  public int getValue() {
    return this.value;
  }
  
  public boolean isValid() { return true; }
  
}

class MergeTile extends AGamePiece {
  IGamePiece piece1;
  IGamePiece piece2;
  MergeTile(IGamePiece piece1, IGamePiece piece2) {
    this.piece1 = piece1;
    this.piece2 = piece2;
  }
  
  public int getValue() {
    return this.piece1.getValue() + this.piece2.getValue();
  }
  
  public boolean isValid() {
    return this.piece1.getValue() == this.piece2.getValue();
  }
  
}

class ExampleTiles {
  
  BaseTile a = new BaseTile(2);
  MergeTile b = new MergeTile(a,a);
  
  IGamePiece four = new MergeTile(new BaseTile(2), new BaseTile(2));
  boolean testGetValue(Tester t) {
    return t.checkExpect(four.getValue(), 4);
  }
}
  