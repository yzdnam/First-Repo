import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import tester.*;
import javalib.impworld.*;
import java.awt.Color;
import javalib.worldimages.*;

class ExampleMaze { 
  void testMaze(Tester t) {
    Maze m = new Maze(20, 15, 40);
    m.bigBang(m.SCENE_WIDTH, m.SCENE_HEIGHT, 0.01);
  } 
}

class Vertex {
  int x;
  int y;
  Vertex(int x, int y) {
    this.x = x;
    this.y = y;
  }
  
  public boolean equals(Object other) {
    if (!(other instanceof Vertex)) { return false; }
    Vertex that = (Vertex)other;
    return this.x == that.x &&
        this.y == that.y;
  }
  
  public int x() { return x; }
  public int y() { return y; }
}

class Edge {
  Vertex from;
  Vertex to;
  boolean drawn;
  Edge(Vertex from, Vertex to) {
    this.from = from;
    this.to = to;
    this.drawn = false;
  }
  
  public boolean equals(Object other) {
    if (!(other instanceof Edge)) { return false; }
    Edge that = (Edge)other;
    return this.from.equals(that.from) &&
        this.to.equals(that.to);
  }
  
  public WorldImage drawAs(Maze maze, Color c) {
    if (from.x() == to.x()) {
      return new LineImage(new Posn(0, maze.edgeSize), c).movePinhole(0, -maze.edgeSize / 2);
    }
    else {
      return new LineImage(new Posn(maze.edgeSize, 0), c).movePinhole(-maze.edgeSize / 2, 0);
    }
  }
    
  public WorldImage draw(Maze maze) {
   if (this.drawn) { 
     return drawAs(maze, Color.BLACK);
   }
   else {
     return drawAs(maze, Color.GRAY);
   }
  }
  
  public WorldImage drawOn(Maze maze) {
      return new OverlayImage(this.draw(maze), 
          maze.mazeImage.movePinholeTo(
              new Posn(from.x() * maze.edgeSize - (maze.MAZE_WIDTH / 2), 
                       from.y() * maze.edgeSize - (maze.MAZE_HEIGHT / 2))));
  }
}

class NodeEdge {
  Node from;
  Node to;
  NodeEdge(Node from, Node to) {
    this.from = from;
    this.to = to;
  }
}

class Node {
  Edge left;
  Edge right;
  Edge top;
  Edge bottom;
  ArrayList<Node> neighbors;
  Color color;
  Color newColor;
  Node(Vertex base, Maze maze) {
    this.left = maze.edges.get(maze.edges.indexOf(new Edge(base, new Vertex(base.x, base.y + 1))));
    this.right = maze.edges.get(maze.edges.indexOf(new Edge(new Vertex(base.x + 1, base.y), new Vertex(base.x + 1, base.y + 1))));
    this.top = maze.edges.get(maze.edges.indexOf(new Edge(base, new Vertex(base.x + 1, base.y))));
    this.bottom = maze.edges.get(maze.edges.indexOf(new Edge(new Vertex(base.x, base.y + 1), new Vertex(base.x + 1, base.y + 1))));
    this.neighbors = new ArrayList<Node>();
    if (base.x == 0 && base.y == 0) {
      this.color = Color.GREEN;
    }
    else if (base.x == maze.width - 1 && base.y == maze.height - 1) {
      this.color = Color.RED;
    }
    else this.color = Color.GRAY;
  }
  
  public boolean equals(Object other) {
    if (!(other instanceof Node)) { return false; }
    Node that = (Node)other;
    return this.left.equals(that.left) &&
        this.right.equals(that.right) &&
        this.top.equals(that.top) &&
        this.bottom.equals(that.bottom);
  }
  
  public WorldImage draw(Maze maze) {
    return new RectangleImage(maze.edgeSize, maze.edgeSize, OutlineMode.SOLID, color);
  }
  
  public WorldImage drawOn(Maze maze) {
    return new OverlayImage(this.draw(maze), 
        maze.mazeImage.movePinholeTo(
            new Posn(left.from.x() * maze.edgeSize + ((maze.edgeSize - maze.MAZE_WIDTH) / 2), 
                     left.from.y() * maze.edgeSize + ((maze.edgeSize - maze.MAZE_HEIGHT) / 2))));
  }
  
  public int x() { return left.from.x(); }
  public int y() { return left.from.y(); }
}

class Maze extends World {
  int width;
  int height;
  int edgeSize;
  ArrayList<ArrayList<Vertex>> vertices;
  ArrayList<Edge> edges;
  ArrayList<Node> nodes;
  ArrayList<Node> nodesToBeAnimated;
  int MAZE_HEIGHT;
  int MAZE_WIDTH;
  int SCENE_WIDTH;
  int SCENE_HEIGHT;
  WorldImage mazeImage;
  Random rand;
  Node origin;
  Node targetNode;
  HashMap<Node, NodeEdge> cameFromNode;
  Maze(int width, int height, int edgeSize) {
    if (width <= 0 || height <= 0 || edgeSize <= 0) { throw new IllegalArgumentException("Maze parameters must be positive integers"); }
    else {
      this.width = width;
      this.height = height;
      this.edgeSize = edgeSize;
      this.vertices = new ArrayList<ArrayList<Vertex>>();
      this.edges = new ArrayList<Edge>();
      this.nodes = new ArrayList<Node>();
      this.nodesToBeAnimated = new ArrayList<Node>();
      this.MAZE_HEIGHT = height * edgeSize;
      this.MAZE_WIDTH = width * edgeSize;
      this.SCENE_WIDTH = MAZE_WIDTH;
      this.SCENE_HEIGHT = MAZE_HEIGHT;
      this.mazeImage = new RectangleImage(MAZE_WIDTH, MAZE_HEIGHT, OutlineMode.SOLID, Color.GRAY);
      rand = new Random();
      
      populateVertices();
      populateEdges();
      setMaze();
      populateNodes();
      drawPath();
      drawMaze();
      origin = nodes.get(0);
      targetNode = nodes.get(nodes.size() - 1);
      cameFromNode = new HashMap<Node, NodeEdge>();
    }
  }
  
  public void onTick() { 
    if (!nodesToBeAnimated.isEmpty()) {
      Node animatingNode = nodesToBeAnimated.remove(0);
      animatingNode.color = animatingNode.newColor;
      animatingNode.drawOn(this);
      mazeImage = animatingNode.drawOn(this).movePinholeTo(new Posn(0, 0));
      if (animatingNode.left.drawn) { 
        mazeImage = animatingNode.left.drawOn(this).movePinholeTo(new Posn(0, 0));
      }
      if (animatingNode.right.drawn) { 
        mazeImage = animatingNode.right.drawOn(this).movePinholeTo(new Posn(0, 0));
      }
      if (animatingNode.top.drawn) { 
        mazeImage = animatingNode.top.drawOn(this).movePinholeTo(new Posn(0, 0)); 
      }
      if (animatingNode.bottom.drawn) { 
      mazeImage = animatingNode.bottom.drawOn(this).movePinholeTo(new Posn(0, 0));
      }
    }
    else if (!origin.color.equals(Color.blue)) {
      reconstruct(origin, targetNode);
      drawPath();
      drawMaze();
    }
  }
  
  public WorldScene makeScene() {
    WorldScene scene = new WorldScene(SCENE_WIDTH, SCENE_HEIGHT);
    scene.placeImageXY(mazeImage, SCENE_WIDTH / 2, SCENE_HEIGHT / 2);
    return scene;
  }
  
  public void onKeyEvent(String key) {
    if (key.equals("B")) {
      bfPath(origin, targetNode);
    }
    else if (key.equals("D")) {
      dfPath(origin, targetNode);
    }
  }
  
  void populateVertices() {
    int w;
    int h;
    for (w = 0;
        w <= width;
        w = w + 1) {
      vertices.add(new ArrayList<Vertex>());
      for (h = 0;
          h <= height;
          h = h + 1) {
        vertices.get(w).add(new Vertex(w, h));
      }
    }
  }
  
  void populateEdges() {
    for (ArrayList<Vertex> column : vertices) {
      for (Vertex v : column) {
        if (v.x != width && v.y != height) {
          edges.add(new Edge(v, vertices.get(v.x).get(v.y + 1)));
          edges.add(new Edge(v, vertices.get(v.x + 1).get(v.y)));
        }
        else if (v.x == width && v.y != height) {
          edges.add(new Edge(v, vertices.get(v.x).get(v.y + 1)));
        }
        else if (v.y == height && v.x != width) {
          edges.add(new Edge(v, vertices.get(v.x + 1).get(v.y)));
        }
      }
    }
  }
  
  void setMaze() {
    RepresentativeMap<Vertex> reps = new RepresentativeMap<Vertex>(allVertices());
    ArrayList<Edge> worklist = new ArrayList<Edge>(this.edges);
    
    for (Edge e : worklist) {
      // if edge is a border, set its drawn value to true and union them on the repmap
      if (e.from.y() == 0 && e.to.y() == 0 || 
          e.from.x() == 0 && e.to.x() == 0 || 
          e.from.y() == height && e.to.y() == height ||
          e.from.x() == width && e.to.x() == width) {
        e.drawn = true;
        reps.union(e.from, e.to);
      }
    }
    
    while (reps.moreThanOneTree()) {
      Edge nextEdge = pickRandomEdge(worklist);
      if (!reps.getRep(nextEdge.from).equals(reps.getRep(nextEdge.to))) {
        nextEdge.drawn = true;
        reps.union(nextEdge.from, nextEdge.to);
      }
    }
  }
  
  ArrayList<Vertex> allVertices() {
    ArrayList<Vertex> result = new ArrayList<Vertex>();
    for (ArrayList<Vertex> column : this.vertices) {
      for (Vertex v : column) {
        result.add(v);
      }
    }
    return result;
  }
  
  Edge pickRandomEdge(ArrayList<Edge> edgeList) {
    return edgeList.remove(rand.nextInt(edgeList.size()));
  }
  
  void populateNodes() {
 // populate nodes and their neighbors
    for (ArrayList<Vertex> column : vertices) {
      for (Vertex v : column) {
        if (!(v.x == width || v.y == height)) {
          nodes.add(new Node(v, this));
        }
      }
    }
    for (Node n : nodes) {
      if (!n.left.drawn) {
        n.neighbors.add(nodes.get(nodes.indexOf(n) - height));
      }
      if (!n.right.drawn) {
        n.neighbors.add(nodes.get(nodes.indexOf(n) + height));
      }
      if (!n.top.drawn) {
        n.neighbors.add(nodes.get(nodes.indexOf(n) - 1));
      }
      if (!n.bottom.drawn) {
        n.neighbors.add(nodes.get(nodes.indexOf(n) + 1));
      }
    }
  }
  
  public void drawPath() {
    for (Node n : nodes) {
      if (!n.color.equals(Color.GRAY)) {
        mazeImage = n.drawOn(this);
      }
    }
    mazeImage = mazeImage.movePinholeTo(new Posn(0, 0));
  }
  
  public void drawMaze() {
    for (Edge e : edges) {
      if (e.drawn) {
        mazeImage = e.drawOn(this);
      }
    }
    mazeImage = mazeImage.movePinholeTo(new Posn(0, 0));
  }
  
  public void bfPath(Node from, Node to) {
    findPath(from, to, new Queue<Node>());
  }
  
  public void dfPath(Node from, Node to) {
    findPath(from, to, new Stack<Node>());
  }
  
  public void findPath(Node from, Node to, ICollection<Node> worklist) {
    Deque<Node> alreadySeen = new Deque<Node>();

    
    worklist.add(from);
    cameFromNode.put(from, new NodeEdge(from, from));
    while(!worklist.isEmpty()) {
      Node next = worklist.remove();
      if (next.equals(to)) {
        return;
      }
      else if (alreadySeen.contains(next)) {
        // do nothing, we've already seen this node
      }
      else {
        next.newColor = Color.YELLOW;
        nodesToBeAnimated.add(next);
        for (Node n : next.neighbors) {
          if (alreadySeen.contains(n)) {
           // do nothing 
          }
          else {
            worklist.add(n);
            cameFromNode.put(n, new NodeEdge(next, n));
          }
        }
        alreadySeen.addAtHead(next);
      }
    }
  }
  
  public void reconstruct(Node origin, Node target) {

    while (!cameFromNode.get(target).from.equals(target)) {
      target.color = Color.blue;
      target = cameFromNode.get(target).from;
    }
    origin.color = Color.blue;
  }
}

class RepresentativeMap<T> extends HashMap<T, T> {
  ArrayList<T> reps;
  RepresentativeMap(ArrayList<T> reps) {
    this.reps = reps;
    for (T rep : reps) {
      super.put(rep, rep);
    }
  }
  
  public T getRep(T v) {
    if (this.get(v).equals(v)) { return v; }
    else return getRep(this.get(v));
  }
  
  public boolean moreThanOneTree() {
    T baseRep = this.getRep(reps.get(0));
    for (Map.Entry<T, T> entry : this.entrySet()) {
      if (!this.getRep(entry.getKey()).equals(baseRep)) {
        return true;
      }
    }
    return false;
  }
  
  public void union(T v1, T v2) {
    this.put(this.getRep(v1), this.getRep(v2));
  }
  
}