import tester.*;

interface IRunnerPredicate {
  boolean apply(Runner r);
}
class RunnerIsMale implements IRunnerPredicate {
  public boolean apply(Runner r) { return r.isMale; }
}
class RunnerIsFemale implements IRunnerPredicate {
  public boolean apply(Runner r) { return !r.isMale; }
}
class RunnerIsInFirst50 implements IRunnerPredicate {
  public boolean apply(Runner r) { return r.pos <= 50; }
}
class RunnerFinishedUnder4Hrs implements IRunnerPredicate {
  public boolean apply(Runner r) { return r.time < 240; }
}

interface IRunnerComparison {
  // returns < 0 if r1 comes before r2 in this order
  // return 0 if r1 is tied with r2
  //returns > 0 if r1 comes after r2 in this order
  int compare(Runner r1, Runner r2);
}
class CompareByTime implements IRunnerComparison {
  public int compare(Runner r1, Runner r2) {
    return r1.time - r2.time;
  }
}
class ReverseComparator implements IRunnerComparison {
  IRunnerComparison comp;
  ReverseComparator(IRunnerComparison comp) {
    this.comp = comp;
  }
  
  public int compare(Runner r1, Runner r2) {
    return this.comp.compare(r1, r2) * -1;
  }
}

class AndPredicate implements IRunnerPredicate {
  IRunnerPredicate left, right;
  AndPredicate(IRunnerPredicate left, IRunnerPredicate right) {
    this.left = left;
    this.right = right;
  }
  public boolean apply(Runner r) {
    return this.left.apply(r) && this.right.apply(r);
  }
}

class OrPredicate implements IRunnerPredicate {
  IRunnerPredicate left, right;
  OrPredicate(IRunnerPredicate left, IRunnerPredicate right) {
    this.left = left;
    this.right = right;
  }
  public boolean apply(Runner r) {
    return this.left.apply(r) || this.right.apply(r);
  }
}

interface ILoRunner {

  ILoRunner find(IRunnerPredicate pred);
  ILoRunner sortBy(IRunnerComparison pred);
  ILoRunner insertBy(IRunnerComparison pred, Runner r);
  ILoRunner sortByTime();
  ILoRunner insertByTime(Runner r);
  //Finds the fastest Runner in this list of Runners
  Runner findWinner();
  //Finds the first Runner in this list of Runners
  Runner getFirst();
  Runner findMax(IRunnerComparison comp);
  Runner findMin(IRunnerComparison comp);
  Runner findMinHelp(IRunnerComparison comp, Runner r1);

}

class Runner {
  String name;
  int age;
  int bib;
  boolean isMale;
  int pos;
  int time;
  Runner(String name, int age, int bib, boolean isMale, int pos, int time) {
    this.name = name;
    this.age = age;
    this.bib = bib;
    this.isMale = isMale;
    this.pos = pos;
    this.time = time;
  } 

}

class ConsLoRunner implements ILoRunner {
  Runner first;
  ILoRunner rest;
  ConsLoRunner(Runner first, ILoRunner rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public Runner findMax(IRunnerComparison comp) {
    return findMin(new ReverseComparator(comp));
  }
  
  public Runner findMin(IRunnerComparison comp) {
    return this.rest.findMinHelp(comp, this.first);
  }
  
  public Runner findMinHelp(IRunnerComparison comp, Runner r) {
    if (comp.compare(first, r) < 0) {
      return this.rest.findMinHelp(comp, first);
    }
    else {
      return this.rest.findMinHelp(comp, r);
    }
  }
  
  public Runner findWinner() {
    return this.findMin(new CompareByTime());
  }
  
  public Runner getFirst() {
    return this.first;
  }
  
  public ILoRunner find(IRunnerPredicate pred) {
    if (pred.apply(this.first)) {
      return new ConsLoRunner(this.first, this.rest.find(pred));
    }
    else {
      return this.rest.find(pred);
    }
  }
  
  public ILoRunner sortBy(IRunnerComparison pred) {
    return this.rest.sortBy(pred).insertBy(pred, this.first);
  }
  
  public ILoRunner insertBy(IRunnerComparison pred, Runner r) {
    if (pred.compare(this.first, r) < 0) {
      return new ConsLoRunner(this.first, this.rest.insertBy(pred, r));
    }
    else return new ConsLoRunner(r, this.rest.insertBy(pred, this.first));
  }
  
  public ILoRunner sortByTime() {
    return this.rest.sortByTime().insertByTime(first);
  }
  
  public ILoRunner insertByTime(Runner r) {
    if (this.first.time > r.time) {
      return new ConsLoRunner(r, this);
    }
    else return new ConsLoRunner(this.first, this.rest.insertByTime(r));
  }
}

class MtLoRunner implements ILoRunner {
  MtLoRunner() {}
  
  public Runner findMax(IRunnerComparison comp) {
    throw new RuntimeException("No maximum runner available in this list!");
  }
  
  public Runner findMin(IRunnerComparison comp) {
    throw new RuntimeException("No minimum runner available in this list!");
  }
  
  public Runner findMinHelp(IRunnerComparison comp, Runner r) {
    return r;
  }
  
  public Runner findWinner() {
    throw new RuntimeException("No winner of an empty list of Runners");
  }
  
  public Runner getFirst() {
    throw new RuntimeException("No first of an empty list of Runners");
  }
  
  public ILoRunner find(IRunnerPredicate pred) { return this; }
  
  public ILoRunner sortBy(IRunnerComparison pred) {
    return this;
  }
  
  public ILoRunner insertBy(IRunnerComparison pred, Runner r) {
    return new ConsLoRunner(r, this);
  }
  
  public ILoRunner sortByTime() {
    return this;
  }
  
  public ILoRunner insertByTime(Runner r) {
    return new ConsLoRunner(r, this);
  }

}

class ExampleRunners {
  Runner johnny = new Runner("Kelly", 97, 999, true, 30, 360);
  Runner frank  = new Runner("Shorter", 32, 888, true, 245, 130);
  Runner bill = new Runner("Rogers", 36, 777, true, 119, 129);
  Runner joan = new Runner("Benoit", 29, 444, false, 18, 155);
   
  ILoRunner mtlist = new MtLoRunner();
  ILoRunner list1 = new ConsLoRunner(johnny, new ConsLoRunner(joan, mtlist));
  ILoRunner list2 = new ConsLoRunner(frank, new ConsLoRunner(bill, list1));
  ILoRunner sortedList2 = new ConsLoRunner(bill, new ConsLoRunner(frank, new ConsLoRunner(joan, new ConsLoRunner(johnny, mtlist))));
  
  boolean testFirst50(Tester t) {
    return
        t.checkExpect(list2.find(new RunnerIsInFirst50()), new ConsLoRunner(johnny, new ConsLoRunner(joan, mtlist))) &&
        t.checkExpect(list2.find(new RunnerFinishedUnder4Hrs()), new ConsLoRunner(frank, new ConsLoRunner(bill, new ConsLoRunner(joan, mtlist)))) &&
        t.checkExpect(list2.find(new AndPredicate(new RunnerIsMale(), new RunnerFinishedUnder4Hrs())), 
            new ConsLoRunner(frank, new ConsLoRunner(bill, mtlist))) &&
        t.checkExpect(list2.find(new OrPredicate(new RunnerFinishedUnder4Hrs(), new RunnerIsFemale())), 
            new ConsLoRunner(frank, new ConsLoRunner(bill, new ConsLoRunner(joan, mtlist))));
  }
  
  boolean testSortByTime(Tester t) {
    return 
        t.checkExpect(list2.sortByTime(), new ConsLoRunner(bill, new ConsLoRunner(frank, new ConsLoRunner(joan, new ConsLoRunner(johnny, mtlist)))))
        && t.checkExpect(list2.sortBy(new CompareByTime()), this.sortedList2);
  }
  
  boolean testFindWinner(Tester t) {
    return
        t.checkExpect(list2.findWinner(), bill) &&
        t.checkExpect(list2.findMax(new CompareByTime()), johnny);
  }
}