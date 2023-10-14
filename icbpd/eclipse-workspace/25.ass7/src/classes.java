interface IArith {
  <R> R accept(IArithVisitor<R> visitor);
}

class Const implements IArith {
  double num;
  Const(double num) {
    this.num = num;
  }
  
  public <R> R accept(IArithVisitor<R> visitor) {
    return visitor.visitConst(this);
  }
}

class UnaryFormula implements IArith {
  Function<Double, Double> func;
  String name;
  IArith child;
  UnaryFormula(Function<Double, Double> func, String name, IArith child) {
    this.func = func;
    this.name = name;
    this.child = child;
  }
  
  public <R> R accept(IArithVisitor<R> visitor) {
    return visitor.visitUnaryFormula(this);
  }
}

/* class Negate extends UnaryFormula {
  Negate(IArith child) {
    super(new NegateFunc(), "neg", child);
  }
}

class Square extends UnaryFormula {
  Square(IArith child) {
    super(new SquareFunc(), "sqr", child);
  }
} */

class BinaryFormula implements IArith {
  BiFunction<Double, Double, Double> func;
  String name;
  IArith left;
  IArith right;
  BinaryFormula(BiFunction<Double, Double, Double> func, String name, IArith left, IArith right) {
    this.func = func;
    this.name = name;
    this.left = left;
    this.right = right;
  }
  
  public <R> R accept(IArithVisitor<R> visitor) {
    return visitor.visitBinaryFormula(this);
  }
}

/* class Plus extends BinaryFormula {
  Plus(IArith left, IArith right) {
    super(new PlusFunc(), "plus", left, right);
  }
}

class Minus extends BinaryFormula {
  Minus(IArith left, IArith right) {
    super(new MinusFunc(), "minus", left, right);
  }
}

class Multiply extends BinaryFormula {
  Multiply(IArith left, IArith right) {
    super(new MultiplyFunc(), "mul", left, right);
  }
}

class Divide extends BinaryFormula {
  Divide(IArith left, IArith right) {
    super(new DivideFunc(), "div", left, right);
  }
} */