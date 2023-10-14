interface IPred<T> {
  boolean apply(T t);
}

interface IPred2<T> {
  boolean apply(T t1, T t2);
}

interface IPred3<T, U> {
  boolean apply(T t, U u);
}

interface IComparator<T> {
  int compare(T t1, T t2);
}

interface IFunc<A, R> {
  R apply(A arg);
}

interface IFunc2<A1, A2, R> {
  R apply(A1 arg1, A2 arg2);
}

interface IList<T> {
  <U> U findSolutionOrElse(IFunc<T, U> convert, IPred<U> pred, U backup);
  
  IList<T> append(IList<T> other);
  IList<T> filter(IPred<T> pred);
  IList<T> filter(IPred2<T> pred, T base);
  <U> IList<T> filter(IPred3<T, U> pred, U base);
  boolean orMap(IPred<T> pred);
  boolean orMap(IPred2<T> pred, T base);
  <U> boolean orMap(IPred3<T, U> pred, U base);
  IList<T> sort(IComparator<T> comp);
  int length();
  IList<T> insert(IComparator<T> comp, T t);
  <U> IList<U> map(IFunc<T, U> f);
  <U> U foldr(IFunc2<T, U, U> f, U base);
  boolean isCons();
  ConsList<T> asCons();
}

class MtList<T> implements IList<T> {
  public <U> U findSolutionOrElse(IFunc<T, U> convert, IPred<U> pred, U backup) { return backup; }
  
  public IList<T> append(IList<T> other) { return other; }
  public IList<T> filter(IPred<T> pred) { return this; }
  public IList<T> filter(IPred2<T> pred, T base) { return this; }
  public <U> IList<T> filter(IPred3<T, U> pred, U base) { return this; }
  public boolean orMap(IPred<T> pred) { return false; }
  public boolean orMap(IPred2<T> pred, T base) { return false; }
  public <U> boolean orMap(IPred3<T, U> pred, U base) { return false; }
  public IList<T> sort(IComparator<T> comp) { return this; }
  public int length() { return 0; }
  public IList<T> insert(IComparator<T> comp, T t) { return new ConsList<T>(t, this); }
  public <U> IList<U> map(IFunc<T, U> f) { return new MtList<U>(); }
  public <U> U foldr(IFunc2<T, U, U> f, U base) { return base; }
  public boolean isCons() { return false; }
  public ConsList<T> asCons() { throw new IllegalArgumentException("asCons() called on an MtList"); }
}

class ConsList<T> implements IList<T> {
  T first;
  IList<T> rest;
  ConsList(T first, IList<T> rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public <U> U findSolutionOrElse(IFunc<T, U> convert, IPred<U> pred, U backup) {
    U firstResult = convert.apply(first);
    if (pred.apply(firstResult)) {
      return firstResult;
    }
    else return rest.findSolutionOrElse(convert, pred, backup); 
  }
  
  public IList<T> append(IList<T> other) {
    return new ConsList<T>(this.first, this.rest.append(other));
  }
  
  public <U> IList<U> map(IFunc<T, U> f) {
    return new ConsList<U>(f.apply(this.first), this.rest.map(f));
  }
  
  public IList<T> filter(IPred<T> pred) {
    if (pred.apply(this.first)) {
      return new ConsList<T>(this.first, this.rest.filter(pred));
    }
    else {
      return this.rest.filter(pred);
    }
  }
  
  public IList<T> filter(IPred2<T> pred, T base) {
    if (pred.apply(this.first, base)) {
      return new ConsList<T>(this.first, this.rest.filter(pred, base));
    }
    else {
      return this.rest.filter(pred, base);
    }
  }
  
  public <U> IList<T> filter(IPred3<T, U> pred, U base) {
    if (pred.apply(this.first, base)) {
      return new ConsList<T>(this.first, this.rest.filter(pred, base));
    }
    else {
      return this.rest.filter(pred, base);
    }
  }
  
  public boolean orMap(IPred<T> pred) {
    if (pred.apply(this.first)) {
      return true;
    }
    else {
      return this.rest.orMap(pred);
    }
  }
  
  public boolean orMap(IPred2<T> pred, T base) {
    if (pred.apply(this.first, base)) {
      return true;
    }
    else {
      return this.rest.orMap(pred, base);
    }
  }
  
  public <U> boolean orMap(IPred3<T, U> pred, U base) {
    if (pred.apply(this.first, base)) {
      return true;
    }
    else {
      return this.rest.orMap(pred, base);
    }
  }
  
  public int length() {
    return 1 + this.rest.length();
  }
  
  public IList<T> sort(IComparator<T> comp) {
    return this.rest.sort(comp).insert(comp, this.first);
  }
  
  public IList<T> insert(IComparator<T> comp, T t) {
    if (comp.compare(this.first, t) > 0) {
      return new ConsList<T>(this.first, this.rest.insert(comp, t));
    }
    else return new ConsList<T>(t, this);
  }
  
  public <U> U foldr(IFunc2<T, U, U> f, U base) {
    return f.apply(first, rest.foldr(f, base));
  }
  
  public boolean isCons() { return true; }
  
  public ConsList<T> asCons() { return this; }
}