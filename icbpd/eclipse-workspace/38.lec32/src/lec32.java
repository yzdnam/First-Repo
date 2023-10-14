import java.util.*;

import tester.Tester;

class ExampleMST {
  Vertex a, b, c, d, e, f;
  Edge ab, ae, be, bc, bf, ec, cd, df;
  Edge ba, ea, eb, cb, fb, ce, dc, fd;
  Graph g;
  ArrayList<Edge> primsAns;
  RepresentativeMap tMap;
  
  void initGraph() {
    a = new Vertex("a");
    b = new Vertex("b");
    c = new Vertex("c");
    d = new Vertex("d");
    e = new Vertex("e");
    f = new Vertex("f");
    ab = new Edge(a, b, 30);
    ae = new Edge(a, e, 50);
    be = new Edge(b, e, 35);
    bc = new Edge(b, c, 40);
    bf = new Edge(b, f, 50);
    ec = new Edge(e, c, 15);
    cd = new Edge(c, d, 25);
    df = new Edge(d, f, 50);
    
    ba = new Edge(b, a, 30);
    ea = new Edge(e, a, 50);
    eb = new Edge(e, b, 35);
    cb = new Edge(c, b, 40);
    fb = new Edge(f, b, 50);
    ce = new Edge(c, e, 15);
    dc = new Edge(d, c, 25);
    fd = new Edge(f, d, 50);
    g = new Graph();
    g.vertices.add(a);
    g.vertices.add(b);
    
    tMap = new RepresentativeMap(g.vertices);
    g.vertices.add(c);
    g.vertices.add(d);
    g.vertices.add(e);
    g.vertices.add(f); 
    
    primsAns = new ArrayList<Edge>();
    primsAns.add(ab);
    primsAns.add(be);
    primsAns.add(bf);
    primsAns.add(ec);
    primsAns.add(cd);
    
    
  }
  
/*  void testCheapestEdge(Tester t) {
    initGraph();
    t.checkExpect(g.cheapestEdge(), ce);
  } */
  
  void testPrims(Tester t) {
    initGraph();

    t.checkExpect(new ArrayUtils().weight(g.primMST()), 155);
    t.checkExpect(new ArrayUtils().allVertices(g.primMST(), g.vertices), true); 
    t.checkExpect(tMap.getRep("a"), "a");
    t.checkExpect(tMap.moreThanOneTree(), true);
    tMap.union("a", "b");
    t.checkExpect(tMap.moreThanOneTree(), false);
//    t.checkExpect(new Heap<Edge>(g.allEdges(), new LessExpensiveEdge()), new ArrayList<Edge>());
    t.checkExpect(new ArrayUtils().weight(g.kruskalMST()), 155);
    t.checkExpect(new ArrayUtils().allVertices(g.kruskalMST(), g.vertices), true);
  }
}

class Vertex {
  String name;
  ArrayList<Edge> outEdges;
  Vertex(String name) {
    this.name = name;
    this.outEdges = new ArrayList<Edge>();
  }
}
class Edge {
  Vertex from;
  Vertex to;
  int weight;
  Edge(Vertex from, Vertex to, int weight) {
    this.from = from;
    this.to = to;
    this.weight = weight;
    from.outEdges.add(this);
  }
}
class Graph {
  ArrayList<Vertex> vertices;
  Graph() {
    this.vertices = new ArrayList<Vertex>();
  }
  
  ArrayList<Edge> allEdges() {
    ArrayList<Edge> edges = new ArrayList<Edge>();
    for (Vertex v : vertices) {
      for (Edge e : v.outEdges) {
        edges.add(e);
      }
    }
    return edges;
  }
  
  Edge cheapestEdge() {
    Heap<Edge> heapedEdges = new Heap<Edge>(this.allEdges(), new LessExpensiveEdge());
    return heapedEdges.heap.get(0);
  }
  
  ArrayList<Edge> primMST() {
    ArrayList<Edge> result = new ArrayList<Edge>();
    HashMap<Vertex, Boolean> connectedVertices = new HashMap<Vertex, Boolean>();
    for (Vertex v : this.vertices) {
      connectedVertices.put(v, false);
    }
    Edge initialEdge = this.cheapestEdge();
    result.add(initialEdge);
    connectedVertices.put(initialEdge.from, true);
    connectedVertices.put(initialEdge.to, true);
    ArrayList<Edge> initCandidateEdges = new ArrayList<Edge>(initialEdge.from.outEdges);
    initCandidateEdges.addAll(initialEdge.to.outEdges);
    Heap<Edge> candidateEdges = new Heap<Edge>(initCandidateEdges, new LessExpensiveEdge());
    
    while (connectedVertices.containsValue(false)) {
      Edge nextEdge = candidateEdges.remove();
      if (!connectedVertices.get(nextEdge.to)) {
        result.add(nextEdge);
        connectedVertices.put(nextEdge.to, true);
        candidateEdges.add(nextEdge.to.outEdges);
      }
    }
    return result;
  }
  
  ArrayList<Edge> kruskalMST() {
    RepresentativeMap reps = new RepresentativeMap(this.vertices);
    ArrayList<Edge> edgesInTree = new ArrayList<Edge>();
    Heap<Edge> worklist = new Heap<Edge>(this.allEdges(), new LessExpensiveEdge());
    
    while (reps.moreThanOneTree()) {
      Edge nextEdge = worklist.remove();
      if (!reps.getRep(nextEdge.from.name).equals(reps.getRep(nextEdge.to.name))) {
        edgesInTree.add(nextEdge);
        reps.union(nextEdge.from.name, nextEdge.to.name);
      }
    }
    return edgesInTree;
  }
} 

class LessExpensiveEdge implements IComparator<Edge> {
  public int compare(Edge e1, Edge e2) {
    return e2.weight - e1.weight;
  }
}

class RepresentativeMap extends HashMap<String, String> {
  ArrayList<Vertex> vertices;
  RepresentativeMap(ArrayList<Vertex> vertices) {
    this.vertices = vertices;
    for (Vertex v : this.vertices) {
      super.put(v.name, v.name);
    }
  }
  
  public String getRep(String key) {
    if (this.get(key).equals(key)) { return key; }
    else return getRep(this.get(key));
  }
  
  public boolean moreThanOneTree() {
    String baseRep = this.getRep(vertices.get(0).name);
    for (Map.Entry<String, String> entry : this.entrySet()) {
      if (!this.getRep(entry.getKey()).equals(baseRep)) {
        return true;
      }
    }
    return false;
  }
  
  public void union(String s1, String s2) {
    this.put(this.getRep(s1), this.getRep(s2));
  }
  
}