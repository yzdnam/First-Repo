interface IArithVisitor<R> extends Function<IArith, R> {
  R visitConst(Const c);
  R visitUnaryFormula(UnaryFormula u);
  R visitBinaryFormula(BinaryFormula b);
}

class EvalVisitor implements IArithVisitor<Double> {
  public Double visitConst(Const c) { return c.num; }
  public Double visitUnaryFormula(UnaryFormula u) { return u.func.apply(new EvalVisitor().apply(u.child)); }
  public Double visitBinaryFormula(BinaryFormula b) { return b.func.apply(new EvalVisitor().apply(b.left), new EvalVisitor().apply(b.right)); }
  
  public Double apply(IArith a) { return a.accept(this); }
}

class PrintVisitor implements IArithVisitor<String> {
  public String visitConst(Const c) { return Double.toString(c.num); }
  public String visitUnaryFormula(UnaryFormula u) { return "(" + u.name + " " + new PrintVisitor().apply(u.child) + ")"; }
  public String visitBinaryFormula(BinaryFormula b) {
    return "(" + b.name + " " + new PrintVisitor().apply(b.left) + " " + new PrintVisitor().apply(b.right) + ")";
  }
  
  public String apply(IArith a) { return a.accept(this); }
}

class DoublerVisitor implements IArithVisitor<IArith> {
  public IArith visitConst(Const c) { return new Const(c.num * 2); }
  public IArith visitUnaryFormula(UnaryFormula u) { return new UnaryFormula(u.func, u.name, new DoublerVisitor().apply(u.child)); }
  public IArith visitBinaryFormula(BinaryFormula b) {
    return new BinaryFormula(b.func, b.name, new DoublerVisitor().apply(b.left), new DoublerVisitor().apply(b.right)); 
  }
  
  public IArith apply(IArith a) { return a.accept(this); }
}

class NoNegativeResults implements IArithVisitor<Boolean> {
  public Boolean visitConst(Const c) { return c.num > 0; }
  public Boolean visitUnaryFormula(UnaryFormula u) {
    return new EvalVisitor().apply(u) > 0 && new NoNegativeResults().apply(u.child);
  }
  public Boolean visitBinaryFormula(BinaryFormula b) {
    return new EvalVisitor().apply(b) > 0 && new NoNegativeResults().apply(b.left) && new NoNegativeResults().apply(b.right);
  }
  
  public Boolean apply(IArith a) { return a.accept(this); }
}