import tester.*;

class Examples {
  BinaryFormula plus = new BinaryFormula(new PlusFunc(), "plus", new Const(1.0), new Const(2.0));
  UnaryFormula neg = new UnaryFormula(new NegateFunc(), "neg", new Const(1.5));
  BinaryFormula div = new BinaryFormula(new DivideFunc(), "div", plus, neg);
  
  void testEval(Tester t) {
    t.checkExpect(new EvalVisitor().apply(plus), 3.0);
    t.checkExpect(new EvalVisitor().apply(neg), -1.5);
    t.checkExpect(new EvalVisitor().apply(div), -2.0);
  }
  
  void testPrint(Tester t) {
    t.checkExpect(new PrintVisitor().apply(div), "(div (plus 1.0 2.0) (neg 1.5))");
  }
  
  void testNegativeResults(Tester t) {
    t.checkExpect(new NoNegativeResults().apply(div), false);
    t.checkExpect(new NoNegativeResults().apply(plus), true);
  }
}