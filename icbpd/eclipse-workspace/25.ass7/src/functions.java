interface Function<A, R> {
  R apply(A arg);
}

class NegateFunc implements Function<Double, Double> {
  public Double apply(Double arg) { return arg * -1; }
}

class SquareFunc implements Function<Double, Double> {
  public Double apply(Double arg) { return Math.pow(arg, 2); }
}

interface BiFunction<A1, A2, R> {
  R apply(A1 arg1, A2 arg2);
}

class PlusFunc implements BiFunction<Double, Double, Double> {
  public Double apply(Double arg1, Double arg2) { return arg1 + arg2; }
}

class MinusFunc implements BiFunction<Double, Double, Double> {
  public Double apply(Double arg1, Double arg2) { return arg1 - arg2; }
}

class MultiplyFunc implements BiFunction<Double, Double, Double> {
  public Double apply(Double arg1, Double arg2) { return arg1 * arg2; }
}

class DivideFunc implements BiFunction<Double, Double, Double> {
  public Double apply(Double arg1, Double arg2) { return arg1 / arg2; }
}