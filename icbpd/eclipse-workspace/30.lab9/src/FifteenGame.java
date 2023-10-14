import java.util.ArrayList;
import tester.*;
import javalib.impworld.*;
import javalib.worldimages.*;
import java.awt.Color;
import java.util.*;

class ExampleFifteenGame {
  void testGame(Tester t) {
    FifteenGame g = new FifteenGame();
    g.bigBang(120, 120);
  } 
  
/*  void testSwap(Tester t) {
    FifteenGame g = new FifteenGame();
//   t.checkExpect(g.tiles, new ArrayList<ArrayList<Tile>>());
   g.onKeyEvent("right");
//    g.tiles.get(0).remove(0);
    t.checkExpect(g.tiles, new ArrayList<ArrayList<Tile>>());
  }  */
}

interface Consts {
  
  int TILE_SIZE = 20;
  int TILE_OFFSET = (int)(TILE_SIZE * 1.5);
  int TEXT_SIZE = TILE_SIZE / 2;
  int BOARD_SIZE = TILE_SIZE * 4;
  int SCENE_HEIGHT = 120;
  int SCENE_WIDTH = SCENE_HEIGHT;
  int SCENE_CENTER = SCENE_HEIGHT / 2;
}

//Represents an individual tile
class Tile implements Consts { 
//The number on the tile.  Use 0 to represent the hole
  int value;
  int x;
  int y;
  Tile(int value, int x, int y) {
    this.value = value;
    this.x = x;
    this.y = y;
  }
  //Draws a tile
  WorldImage draw() {
    return new OverlayImage(new TextImage(String.valueOf(value), TEXT_SIZE, FontStyle.BOLD, Color.BLACK), 
        new RectangleImage(TILE_SIZE, TILE_SIZE, OutlineMode.SOLID, Color.RED));
  }
//Draws this tile onto the background at the specified logical coordinates
  WorldImage drawAt(int row, int col, WorldImage background) { 
    if (value == 0) {
      return background;
    }
    else
    return new OverlayImage(this.draw(), background.movePinholeTo(new Posn(row * TILE_SIZE - TILE_OFFSET, col * -TILE_SIZE + TILE_OFFSET)));
  }
}

class FifteenGame extends World implements Consts {
//represents the rows of tiles
  ArrayList<ArrayList<Tile>> tiles;
  ArrayList<Tile> tileRows;
  ArrayList<String> movesHistory;
  Random rand = new Random();
  ArrayList<Integer> tileValuesToAssign;
  int tilesToAssign = 16;
  int row;
  int col;
  WorldScene scene;
  WorldImage board;

  FifteenGame() {
    scene = new WorldScene(SCENE_HEIGHT, SCENE_WIDTH);
    movesHistory = new ArrayList<String>();
    tileValuesToAssign = new ArrayList<Integer>();
    int i;
    for (i = 0;
        i < 16;
        i = i + 1) {
      tileValuesToAssign.add(i);
    }
    
    tiles = new ArrayList<ArrayList<Tile>>();
    for (col = 0;
        col < 4;
        col = col + 1) {
      tiles.add(new ArrayList<Tile>(4));
    }
    
    for (col = 0;
        col < 4;
        col = col + 1) {
      for (row = 0;
          row < 4;
          row = row + 1) {
        tiles.get(row).add(new Tile(tileValuesToAssign.remove(rand.nextInt(tilesToAssign)), row, col));
        tilesToAssign = tilesToAssign - 1;
      }
    } 
  }
//draws the game
  public WorldScene makeScene() { 
    board = new RectangleImage(BOARD_SIZE, BOARD_SIZE, OutlineMode.SOLID, Color.GRAY);
    for (ArrayList<Tile> rowList : tiles) {
      for (Tile tile : rowList) {
        board = tile.drawAt(tile.x, tile.y, board);
      }
    }
    board = board.movePinholeTo(new Posn(0, 0));
    scene.placeImageXY(board, SCENE_CENTER, SCENE_CENTER);
    return scene;
  }
  
//handles keystrokes
  public void onKeyEvent(String k) {
    // needs to handle up, down, left, right to move the hole
    // extra: handle "u" to undo moves
    Posn holePosn = getTilePosnByVal(0);
    Tile hole = tiles.get(holePosn.x).get(holePosn.y);
    if (k.compareTo("up") == 0) {
      if (hole.y == 3) { return; }
      else { 
        swap(hole, tiles.get(hole.x).get(hole.y + 1));
        movesHistory.add(0, "up");
      }
      }
    else if (k.compareTo("down") == 0) {
      if (hole.y == 0) { return; }
      else { 
        swap(hole, tiles.get(hole.x).get(hole.y - 1));
        movesHistory.add(0, "down");
      }
    }
    else if (k.compareTo("left") == 0) {
      if (hole.x == 0) { return; }
      else { 
        swap(hole, tiles.get(hole.x - 1).get(hole.y));
        movesHistory.add(0, "left");
      }
    }
    else if (k.compareTo("right") == 0) {
      if (hole.x == 3) { return; }
      else { 
        swap(hole, tiles.get(hole.x + 1).get(hole.y));
        movesHistory.add(0, "right");
      }
    }
    else if (k.compareTo("u") == 0 && !movesHistory.isEmpty()) {
      onKeyEventReverse(movesHistory.remove(0));
    }
  }
  
  public void onKeyEventReverse(String k) {
    if (k.compareTo("up") == 0) {
      onKeyEvent("down");
    }
    else if (k.compareTo("down") == 0) {
      onKeyEvent("up");
    }
    else if (k.compareTo("left") == 0) {
      onKeyEvent("right");
    }
    else if (k.compareTo("right") == 0) {
      onKeyEvent("left");
    }
    movesHistory.remove(0);
  }
  
  public Posn getTilePosnByVal(int val) {
    Posn ans = new Posn(-1, -1);
    Tile candidate;
    int col;
    int row;
    for (row = 0;
        row < 4;
        row = row + 1) {
      for (col = 0;
          col < 4;
          col = col + 1) {
        candidate = tiles.get(row).get(col);
        if (candidate.value == val) {
          ans = new Posn(candidate.x, candidate.y);
        }
      }
    }
    return ans;
  }
  
  //swap tiles
  public void swap(Tile tile1, Tile tile2) {
    tiles.get(tile1.x).remove(tile1.y);
    tiles.get(tile1.x).add(tile1.y, new Tile(tile2.value, tile1.x, tile1.y));
    tiles.get(tile2.x).remove(tile2.y);
    tiles.get(tile2.x).add(tile2.y, new Tile(tile1.value, tile2.x, tile2.y));
  }
}