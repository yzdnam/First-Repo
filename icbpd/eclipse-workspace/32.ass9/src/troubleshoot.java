import tester.*;
import javalib.impworld.*;
import javalib.worldimages.*;
import java.awt.Color;
import java.util.*;

class MouseClick {
  void testtroubleshoot(Tester t) {
    MouseClickTroubleshoot m = new MouseClickTroubleshoot();
    m.bigBang(100, 100);
  }
}

class MouseClickTroubleshoot extends World {
  Posn lastClickedPosn;
  MouseClickTroubleshoot() {
    lastClickedPosn = new Posn(0, 0);
  }
  
  public WorldScene makeScene() {
    WorldScene scene = new WorldScene(100, 100);
    scene.placeImageXY(new TextImage(lastClickedPosn.x + " ," + lastClickedPosn.y, 24, FontStyle.BOLD, Color.red), 50, 50);
    return scene;
  }
  
  public void onMouseClicked(Posn pos) {
    lastClickedPosn = pos;
  }
}


