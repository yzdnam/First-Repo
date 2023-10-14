import java.util.ArrayList;
import tester.*;
import javalib.impworld.*;
import java.awt.Color;
import javalib.worldimages.*;
import java.util.*;

class ExampleFloodIt {
  
  void testPosnConversion(Tester t) {
    FloodItWorld g = new FloodItWorld(10, 8);
    t.checkExpect(g.convertPosnToCellPosn(new Posn(-59, -90)), new Cell(2, 0, Color.black));
    t.checkExpect(g.convertPosnToCellPosn(new Posn(0, 0)), new Cell(5, 5, Color.black));
    ArrayList<Cell> cellList = new ArrayList<Cell>();
    cellList.add(new Cell(2, 0, Color.black));
    cellList.add(new Cell(2, 0, Color.black));
    cellList.add(new Cell(2, 0, Color.black));
    cellList.add(new Cell(5, 5, Color.black));
    t.checkExpect(new ArrayUtils().binarySearch(cellList, new Cell(5, 5, Color.red), new CompareCells()), 3);
   // t.checkExpect(g.board, new ArrayList<Cell>());
  }
  void testGame(Tester t) {
    FloodItWorld g = new FloodItWorld(10, 4);
    g.bigBang(g.SCENE_HEIGHT, g.SCENE_WIDTH);
  } 
}
  
class ColorList {
  ArrayList<Color> colors;
  Random rand = new Random();
  ColorList() {
    colors = new ArrayList<Color>();
    colors.add(Color.orange);
    colors.add(Color.cyan);
    colors.add(Color.blue);
    colors.add(Color.red);
    colors.add(Color.pink);
    colors.add(Color.yellow);
    colors.add(Color.green);
    colors.add(Color.magenta);
  }
  
  public Color getRandColor(int numberOfColors) {
    return colors.get(rand.nextInt(numberOfColors));
  }
}

interface Consts {
  
  int TILE_SIZE = 20;
  int TILE_OFFSET = (int)(TILE_SIZE * 1.5);
  int BORDER_SIZE = 50;
  
}

//Represents a single square of the game area
class Cell implements Consts {
//In logical coordinates, with the origin at the top-left corner of the screen
  int x;
  int y;
  Color color;
  boolean flooded;
//the four adjacent cells to this one
  Cell left;
  Cell top;
  Cell right;
  Cell bottom;
  Cell(int x, int y, Color color) {
    this.x = x;
    this.y = y;
    this.color = color;
    this.flooded = false;
  } 
  public WorldImage draw() {
    return new RectangleImage(TILE_SIZE, TILE_SIZE, OutlineMode.SOLID, color);
  }
  
  public WorldImage drawAt(int x, int y, int board_offset, WorldImage background) {
    return new OverlayImage(this.draw(), 
        background.movePinholeTo(new Posn(x * TILE_SIZE + ((TILE_SIZE - board_offset) / 2), y * TILE_SIZE + ((TILE_SIZE - board_offset) / 2))));
  }
  
  public void connect(ArrayList<Cell> allCells) {
    if (x == 0) {
      left = new Cell(-1, -1, Color.black);
    }
    if (y == (Math.sqrt(allCells.size()) - 1)) {
      bottom = new Cell(-1, -1, Color.black);
    }
    if (y == 0) {
      top = new Cell(-1, -1, Color.black);
    }
    if (x == (Math.sqrt(allCells.size()) - 1)) {
      right = new Cell(-1, -1, Color.black);
    }
    for (Cell iCell : allCells) {
      if (iCell.x == this.x - 1 && iCell.y == this.y) {
        this.left = iCell;
      }
      else if (iCell.x == this.x + 1 && iCell.y == this.y) {
        this.right = iCell;
      }
      else if (iCell.x == this.x && iCell.y == this.y - 1) {
        this.top = iCell;
      }
      else if (iCell.x == this.x && iCell.y == this.y + 1) {
        this.bottom = iCell;
      }
    }
  }
  
  public boolean nextToFlooded() {
      return (left.flooded || right.flooded || top.flooded || bottom.flooded);
  }
}


class FloodItWorld extends World implements Consts {
//All the cells of the game
  int BOARD_SIZE_TOTAL;
  int SCENE_HEIGHT;
  int SCENE_WIDTH;
  int SCENE_CENTER;
  int board_size;
  int colors;
  int movesLeft;
  int movesMade;
  ColorList cList;
  ArrayList<Cell> board;
  WorldImage boardImage;
  FloodItWorld(int board_size, int colors) {
    this.board_size = board_size;
    this.colors = colors;
    this.movesLeft = (int)(Math.round(board_size * 0.85) + ((colors - 3) * Math.floor(board_size / 3)));
    this.movesMade = 0;
    
    BOARD_SIZE_TOTAL = TILE_SIZE * board_size;
    SCENE_HEIGHT = BOARD_SIZE_TOTAL + BORDER_SIZE;
    SCENE_WIDTH = SCENE_HEIGHT;
    SCENE_CENTER = SCENE_HEIGHT / 2;
    
    
    cList = new ColorList();
    setBoard();
   
  }
  
  public WorldScene makeScene() { 
    WorldScene scene = new WorldScene(SCENE_WIDTH, SCENE_HEIGHT);
    boardImage = new RectangleImage(BOARD_SIZE_TOTAL, BOARD_SIZE_TOTAL, OutlineMode.SOLID, Color.GRAY);
    WorldImage scoreboard = new TextImage("Moves Made: " + this.movesMade + " Moves Left: " + this.movesLeft, 12, FontStyle.BOLD, Color.BLACK);
    for (Cell iCell : board) {
      boardImage = iCell.drawAt(iCell.x, iCell.y, BOARD_SIZE_TOTAL, boardImage);
    }
    boardImage = boardImage.movePinholeTo(new Posn(0, 0));
    scene.placeImageXY(boardImage, SCENE_CENTER, SCENE_CENTER);
    scene.placeImageXY(scoreboard, SCENE_CENTER, BOARD_SIZE_TOTAL + BORDER_SIZE - board_size);
    return scene;
  }
  
  public void onMouseClicked(Posn pos) {
    if (movesLeft == 0) { return; }
    else {
      movesLeft = movesLeft - 1;
      movesMade = movesMade + 1;
      int clickedCellIdx = new ArrayUtils().binarySearch(board, convertPosnToCellPosn(pos), new CompareCells());
      if (clickedCellIdx < 0 || clickedCellIdx >= 100) { return; }
      else {
        flood(board.get(clickedCellIdx).color);
      }
    }
  }
  
  public void onKeyEvent(String k) {
    if (k.compareTo("r") == 0) {
      setBoard();
    }
  }

  
  public Cell convertPosnToCellPosn(Posn pos) {
    Posn offsetPos = new Posn(pos.x - BORDER_SIZE / 2, pos.y - BORDER_SIZE / 2);
    return new Cell((int)Math.floor(offsetPos.x / TILE_SIZE), 
        (int)Math.floor(offsetPos.y / TILE_SIZE), Color.black);
  }
  
  public void setBoard() {
    board = new ArrayList<Cell>();
    int x;
    int y;
    for (x = 0;
        x < board_size;
        x = x + 1) {
      for (y = 0;
          y < board_size;
          y = y + 1) {
        board.add(new Cell(x, y, cList.getRandColor(colors)));
      }
    }
    for (Cell iCell : board) {
      iCell.connect(board);
    }
    board.get(0).flooded = true;
    flood(board.get(0).color);
  }
  
  public void flood(Color newColor) {
    if (newColor.equals(Color.black)) { return; }
    else {
      for (Cell cell : board) {
        if (cell.flooded) {
          cell.color = newColor;
        }
        else if (cell.nextToFlooded() && cell.color.equals(newColor)) {
          cell.flooded = true;
        }
      }
    }
  }
}

