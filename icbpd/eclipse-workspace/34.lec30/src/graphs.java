import tester.Tester;

class ExampleGraph {
  Graph<Integer> gI;
  Vertex<Integer> one, two, three, four;
  Edge<Integer> oneTwo, twoThree, threeFour;
  Graph<Character> gC;
  Vertex<Character> a, b, c, d, e;
  Edge<Character> aB, bC, bD, cA, eB;
  
  void initGraph() {
    one = new Vertex<Integer>(1);
    two = new Vertex<Integer>(2);
    three = new Vertex<Integer>(3);
    four = new Vertex<Integer>(4);
    oneTwo = new Edge<Integer>(one, two, 1);
    twoThree = new Edge<Integer>(two, three, 1);
    threeFour = new Edge<Integer>(three, four, 1);
    gI = new Graph<Integer>(new ConsList<Vertex<Integer>>(one, new ConsList<Vertex<Integer>>(two, 
        new ConsList<Vertex<Integer>>(three, new ConsList<Vertex<Integer>>(four, new MtList<Vertex<Integer>>())))));
    
    a = new Vertex<Character>('a');
    b = new Vertex<Character>('b');
    c = new Vertex<Character>('c');
    d = new Vertex<Character>('d');
    e = new Vertex<Character>('e');
    aB = new Edge<Character>(a, b, 1);
    bC = new Edge<Character>(b, c, 1);
    bD = new Edge<Character>(b, d, 1);
    cA = new Edge<Character>(c, a, 1);
    eB = new Edge<Character>(e, b, 1);
    gC = new Graph<Character>(new ConsList<Vertex<Character>>(a, new ConsList<Vertex<Character>>(b, 
        new ConsList<Vertex<Character>>(c, new ConsList<Vertex<Character>>(d, 
            new ConsList<Vertex<Character>>(e, new MtList<Vertex<Character>>()))))));
  }
  
  void testAllVertices(Tester t) {
    initGraph();
    t.checkExpect(gI.allEdges(), (new ConsList<Edge<Integer>>(oneTwo, new ConsList<Edge<Integer>>(twoThree, 
        new ConsList<Edge<Integer>>(threeFour, new MtList<Edge<Integer>>())))));
    t.checkExpect(gI.inEdges(two), new ConsList<Vertex<Integer>>(one, new MtList<Vertex<Integer>>()));
    t.checkExpect(a.hasPathTo(e), false);
    t.checkExpect(gC.bfs(a, c), (new ConsList<Vertex<Character>>(c, new ConsList<Vertex<Character>>(b, 
        new ConsList<Vertex<Character>>(a, new MtList<Vertex<Character>>())))));
  }
}

class Vertex<T> {
  T data;
  IList<Edge<T>> outEdges;
  Vertex(T data) {
    this.data = data;
    this.outEdges = new MtList<Edge<T>>();
  }
  boolean hasPathTo(Vertex<T> dest) {
    return hasPathToHelper(dest, new MtList<Vertex<T>>());
  }
        
  boolean hasPathToHelper(Vertex<T> dest, IList<Vertex<T>> pathSoFar) {  
    if (pathSoFar.contains(this)) { return false; }
    else {
      for (Edge<T> e : this.outEdges) {
        if (   e.to == dest            // can get there in just one step
            || e.to.hasPathToHelper(dest, new ConsList<Vertex<T>>(this, pathSoFar))) { // can get there on a path through e.to
          return true;
        }
      }
      return false;
    }
  }
}
class Edge<T> {
  Vertex<T> from;
  Vertex<T> to;
  int weight;
  Edge(Vertex<T> from, Vertex<T> to, int weight) {
    this.from = from;
    this.to = to;
    this.weight = weight;
    from.outEdges = new ConsList<Edge<T>>(this, from.outEdges);
  }
}
class Graph<T> {
  IList<Vertex<T>> allVertices;
  Graph(IList<Vertex<T>> allVertices) {
    this.allVertices = allVertices;
  }
  
  public IList<Edge<T>> allEdges() {
    return allVertices.map(new VertexEdges<T>()).foldr(new AppendLists<T>(), new MtList<Edge<T>>());
  }
  
  public IList<Vertex<T>> inEdges(Vertex<T> v) {
    return this.allVertices.filter(new connects<T>(v));
  }
  
  IList<Vertex<T>> bfs(Vertex<T> from, Vertex<T> to) {
    return searchHelp(from, to, new Queue<ConsList<Vertex<T>>>());
  }
  IList<Vertex<T>> dfs(Vertex<T> from, Vertex<T> to) {
    return searchHelp(from, to, new Stack<ConsList<Vertex<T>>>());
  }
  IList<Vertex<T>> searchHelp(Vertex<T> from, Vertex<T> to, ICollection<ConsList<Vertex<T>>> worklist) {
    Deque<Vertex<T>> alreadySeen = new Deque<Vertex<T>>();
   
    // Initialize the worklist with the from vertex
    worklist.add(new ConsList<Vertex<T>>(from, new MtList<Vertex<T>>()));
    // As long as the worklist isn't empty...
    while (!worklist.isEmpty()) {
      ConsList<Vertex<T>> next = worklist.remove();
      if (next.first.equals(to)) {
        return next; // Success!
      }
      else if (alreadySeen.contains(next.first)) {
        // do nothing: we've already seen this one
      }
      else {
        // add all the neighbors of next to the worklist for further processing
        for (Edge<T> e : next.first.outEdges) {
          worklist.add(new ConsList<Vertex<T>>(e.to, next));
        }
        // add next to alreadySeen, since we're done with it
        alreadySeen.addAtHead(next.first);
      }
    }
    // We haven't found the to vertex, and there are no more to try
    return new MtList<Vertex<T>>();
  }
}

class VertexEdges<T> implements IFunc<Vertex<T>, IList<Edge<T>>> {
  public IList<Edge<T>> apply(Vertex<T> v) { return v.outEdges; }
}
class AppendLists<T> implements IFunc2<IList<Edge<T>>, IList<Edge<T>>, IList<Edge<T>>> {
  public IList<Edge<T>> apply(IList<Edge<T>> l1, IList<Edge<T>> l2) { return l1.append(l2); }
}
class edgeConnects<T> implements IPred3<Edge<T>, Vertex<T>> {
  public boolean apply(Edge<T> e, Vertex<T> v) { return e.to == v; }
}
class connects<T> implements IPred<Vertex<T>> {
  Vertex<T> baseV;
  connects(Vertex<T> baseV) {
    this.baseV = baseV;
  }
  public boolean apply(Vertex<T> iV) { return iV.outEdges.orMap(new edgeConnects<T>(), baseV); }
}