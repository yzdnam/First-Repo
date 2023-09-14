import tester.*;

class Example {
  IList<Integer> lst = new ConsList<Integer>(4, new ConsList<Integer>(6, new ConsList<Integer>(14, new MtList<Integer>())));
  IList<JSON> jlst = new ConsList<JSON>(new JSONBool(true), 
      new ConsList<JSON>(new JSONBlank(), 
          new ConsList<JSON>(new JSONNumber(5), new ConsList<JSON>(new JSONString("fart"), 
              new ConsList<JSON>(new JSONObject(new ConsList<Pair<String, JSON>>(new Pair<String, JSON>("money", new JSONNumber(1)), 
                                                                                new MtList<Pair<String, JSON>>())),
                  new MtList<JSON>())))));
  JSONList jsonlst = new JSONList(jlst);
  
  boolean testFold(Tester t) {
    return t.checkExpect(lst.foldr(new SumList(), 0), 24);
  }
  
  boolean testJSONToNumber(Tester t) {
    return t.checkExpect(jlst.map(new JSONToNumber()), new ConsList<Integer>(1, new ConsList<Integer>(0, new ConsList<Integer>(5, 
        new ConsList<Integer>(4, new ConsList<Integer>(1, new MtList<Integer>()))))));
  }
  
  boolean testFind(Tester t) {
    return t.checkExpect(new JSONFind("money").apply(jsonlst), new JSONNumber(1));
  }
}

class SumList implements IFunc2<Integer, Integer, Integer> {
  public Integer apply(Integer i1, Integer i2) { return i1 + i2; }
}

//a json value
interface JSON {
  Integer acceptToNumber(JSONVisitor<Integer> visitor);
  JSON acceptFind(JSONVisitor<JSON> visitor);
}

//no value
class JSONBlank implements JSON {
  
  public Integer acceptToNumber(JSONVisitor<Integer> visitor) {
    return visitor.visitJSONBlank(this);
  }
  
  public JSON acceptFind(JSONVisitor<JSON> visitor) {
    return visitor.visitJSONBlank(this);
  }
}

//a number
class JSONNumber implements JSON {
  int number;
  
  JSONNumber(int number) {
  this.number = number;
  }
  
  public Integer acceptToNumber(JSONVisitor<Integer> visitor) {
    return visitor.visitJSONNumber(this);
  }
  
  public JSON acceptFind(JSONVisitor<JSON> visitor) {
    return visitor.visitJSONNumber(this);
  }
}

//a boolean
class JSONBool implements JSON {
  boolean bool;
  
  JSONBool(boolean bool) {
    this.bool = bool;
  }
  
  public Integer acceptToNumber(JSONVisitor<Integer> visitor) {
    return visitor.visitJSONBool(this);
  }
  
  public JSON acceptFind(JSONVisitor<JSON> visitor) {
    return visitor.visitJSONBool(this);
  }
}

//a string
class JSONString implements JSON {
  String str;

  JSONString(String str) {
    this.str = str;
  }
  
  public Integer acceptToNumber(JSONVisitor<Integer> visitor) {
    return visitor.visitJSONString(this);
  }
  
  public JSON acceptFind(JSONVisitor<JSON> visitor) {
    return visitor.visitJSONString(this);
  }
}
  
//a list of JSON values
class JSONList implements JSON {
  IList<JSON> values;
 
  JSONList(IList<JSON> values) {
    this.values = values;
  }
  
  public Integer acceptToNumber(JSONVisitor<Integer> visitor) {
    return visitor.visitJSONList(this);
  }
  
  public JSON acceptFind(JSONVisitor<JSON> visitor) {
    return visitor.visitJSONList(this);
  }
}
  
//a list of JSON pairs
class JSONObject implements JSON {
  IList<Pair<String, JSON>> pairs;
  
  JSONObject(IList<Pair<String, JSON>> pairs) {
    this.pairs = pairs;
  }
  
  public Integer acceptToNumber(JSONVisitor<Integer> visitor) {
    return visitor.visitJSONObject(this);
  }
  
  public JSON acceptFind(JSONVisitor<JSON> visitor) {
    return visitor.visitJSONObject(this);
  }
}

//generic pairs
class Pair<X, Y> {
  X x;
  Y y;

 Pair(X x, Y y) {
   this.x = x;
   this.y = y;
 }
}

interface JSONVisitor<T> extends IFunc<JSON, T> {
  T visitJSONBlank(JSONBlank js);
  T visitJSONNumber(JSONNumber js);
  T visitJSONBool(JSONBool js);
  T visitJSONString(JSONString js);
  T visitJSONList(JSONList js);
  T visitJSONObject(JSONObject js);
}

class JSONToNumber implements JSONVisitor<Integer> {
  
  public Integer visitJSONBlank(JSONBlank js) { return 0; }
  
  public Integer visitJSONNumber(JSONNumber js) { return js.number; }
  
  public Integer visitJSONBool(JSONBool js) {
    if (js.bool) {
      return 1;
    }
    else return 0;
  }
  
  public Integer visitJSONString(JSONString js) { return js.str.length(); }
  
  public Integer visitJSONList(JSONList js) { return js.values.map(new JSONToNumber()).foldr(new SumList(), 0); }

  public Integer visitJSONObject(JSONObject js) {
    return this.visitJSONList(new JSONList(js.pairs.map(new PairY())));
  }
  
  public Integer apply(JSON js) { return js.acceptToNumber(this); }
  
}

class JSONFind implements JSONVisitor<JSON> {
  String target;
  JSONFind(String target) {
    this.target = target;
  }
  
  public JSON visitJSONBlank(JSONBlank js) { return js; }
  
  public JSON visitJSONNumber(JSONNumber js) { return new JSONBlank(); }
  
  public JSON visitJSONBool(JSONBool js) { return new JSONBlank(); }
  
  public JSON visitJSONString(JSONString js) { return new JSONBlank(); }
  
  public JSON visitJSONList(JSONList js) { 
    return js.values.map(new JSONFind(target)).findSolutionOrElse(new NoConvert(), new NotBlank(), new JSONBlank());
  }
  
  public JSON visitJSONObject(JSONObject js) {
    return js.pairs.findSolutionOrElse(new MatchTarget(target), new NotBlank(), new JSONBlank());
  }
  
  public JSON apply(JSON js) { return js.acceptFind(this); }
  
}

class PairY implements IFunc<Pair<String, JSON>, JSON> {
  public JSON apply(Pair<String, JSON> p) { return p.y; }
}

class MatchTarget implements IFunc<Pair<String, JSON>, JSON> {
  String target;
  MatchTarget(String target) {
    this.target = target;
  }
  public JSON apply(Pair<String, JSON> p) {
    if (p.x.equals(target)) {
      return p.y;
    }
    else return new JSONBlank();
  }
}

class NoConvert implements IFunc<JSON, JSON> {
  public JSON apply(JSON j) { return j; }
}

class NotBlank implements IPred<JSON> {
  public boolean apply(JSON j) {
    return !(j instanceof JSONBlank);
  }
}