import tester.*;

class ExamplesDeque {
  
  Deque<String> deque1, deque2, deque3;
  Sentinel<String> sentinel2;
  Vertex<String> word1, word2, word3, word4;
  Deque<Integer> deque4;
  
  void initDeques() {
    deque1 = new Deque<String>();
    
    Sentinel<String> sentinel1 = new Sentinel<String>();
    Vertex<String> string2 = new Vertex<String>("bcd");
    Vertex<String> string1 = new Vertex<String>("abc", string2, sentinel1); 
    Vertex<String> string4 = new Vertex<String>("def");
    Vertex<String> string3 = new Vertex<String>("cde", string4, string2);
    string4.next = sentinel1;
    sentinel1.prev = string4;
    deque2 = new Deque<String>(sentinel1);
    
    sentinel2 = new Sentinel<String>();
    word2 = new Vertex<String>("abel");
    word1 = new Vertex<String>("cain", word2, sentinel2); 
    word4 = new Vertex<String>("moses");
    word3 = new Vertex<String>("zeus", word4, word2);
    word4.next = sentinel2;
    sentinel2.prev = word4;
    deque3 = new Deque<String>(sentinel2);

  }
  
  void testSize(Tester t) {
    initDeques();
    t.checkExpect(deque2.size(), 4);
  }
  
  void testAddAtHead(Tester t) {
    initDeques();
    Vertex<String> testVertex = new Vertex<String>("a");
    deque2.addAtHead("a");
    t.checkExpect(deque2.header.getData(deque2.header.next), "a");
    deque2.removeFromHead();
    t.checkExpect(deque2.header.getData(deque2.header.next), "abc");
  }
  
  void testFind(Tester t) {
    initDeques();
    t.checkExpect(deque3.find(new StartsWith("a")), word2);
    t.checkExpect(deque3.find(new StartsWith("b")), sentinel2);
  }
  
  void testRemoveVertex(Tester t) {
    initDeques();
    deque2.removeVertex(word2);
    t.checkExpect(deque3.find(new StartsWith("a")), sentinel2);
  }
}
