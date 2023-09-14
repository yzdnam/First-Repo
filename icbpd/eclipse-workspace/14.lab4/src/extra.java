import tester.*;

interface IMaybeInt {

}

interface ILoI {
  IMaybeInt longestSublist();
  IMaybeInt longestSublistHelp(int currentAns, int currentRun);
  // return the number of times the current first number of a list repeats consecutively
  int currentRun();
  int currentRunHelp(int count);
  // checks if the first of this LoI is the same as the given int
  boolean equalsFirst(int that);
  // returns the LoI after the current run of identical numbers
  ILoI nextChunk();
  // Task problem
  /////////////////////
  boolean allIn(ILoT potentialDependees);
  boolean contains(int that);
  int len();
}

class Int implements IMaybeInt {
  int number;
  Int(int number) {
    this.number = number;
  }
}

class False implements IMaybeInt {
  False() {}
}

class ConsLoI implements ILoI {
  int first;
  ILoI rest;
  ConsLoI(int first, ILoI rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public IMaybeInt longestSublist() {
    return this.longestSublistHelp(this.first, this.currentRun());
  }
  
  public IMaybeInt longestSublistHelp(int currentAns, int currentRun) {
    if (this.nextChunk().currentRun() <= currentRun) {
      return this.nextChunk().longestSublistHelp(currentAns, currentRun);
    }
    else { return this.nextChunk().longestSublist(); }
  }
  
  public int currentRun() {
    return this.currentRunHelp(1);
  }
  
  public int currentRunHelp(int count) {
    if (this.rest.equalsFirst(this.first)) {
      return this.rest.currentRunHelp(count + 1);
    }
    else { return count; }
  }
  
  public boolean equalsFirst(int that) {
    return this.first == that;
  }
  
  public ILoI nextChunk() {
    if (this.rest.equalsFirst(this.first)) {
      return this.rest.nextChunk();
    }
    else { return this.rest; }
  }
  
  // Task problem
  /////////////////////
  
  public boolean allIn(ILoT potentialDependees) {
    return potentialDependees.containsID(this.first) && this.rest.allIn(potentialDependees);
  }
  
  public boolean contains(int that) {
    if (this.first == that) { return true; }
    else return this.rest.contains(that);
  }
  
  public int len() {
    return 1 + this.rest.len();
  }
}

class MtLoI implements ILoI {
  MtLoI() {}
  
  public IMaybeInt longestSublist() {
    return new False();
  }
  
  public IMaybeInt longestSublistHelp(int currentAns, int currentRun) {
    return new Int(currentAns);
  }
  
  public int currentRun() {
    return 0;
  }
  
  public int currentRunHelp(int count) {
    return count;
  }
  
  public boolean equalsFirst(int that) {
    return false;
  }
  
  public ILoI nextChunk() {
    return this;
  }
  
  // Task problem
  /////////////////////
  
  public boolean allIn(ILoT potentialDependees) {
    return true;
  }
  
  public boolean contains(int that) {
    return false;
  }
  
  public int len() {
    return 0;
  }
}

class ExampleSolutions {
  ILoI test = 
      new ConsLoI(1, 
          new ConsLoI(1, 
              new ConsLoI(5, 
                  new ConsLoI(5, 
                      new ConsLoI(5, 
                          new ConsLoI(4, 
                              new ConsLoI(3, new ConsLoI(4, new ConsLoI(4, new ConsLoI(4, new MtLoI()))))))))));
  
  boolean testLongestRun(Tester t) {
    return
        t.checkExpect(this.test.longestSublist(), new Int(5));
  }
}

class Task {
  int id;
  ILoI prereqs;
  Task(int id, ILoI prereqs) {
    this.id = id;
    this.prereqs = prereqs;
  }
  
  // takes in itself, a list of tasks, and another list of tasks
  ILoT findDependees(Task depender, ILoT potentialDependees, ILoT currentAns) {
    if (depender.adequateDependees(potentialDependees)) {
      return new ConsT(this, currentAns);
    }
    else return currentAns;
  }
  
  boolean adequateDependees(ILoT potentialDependees) {
    return this.prereqs.allIn(potentialDependees);
  }
  
  boolean cyclicWithAnyIn(ILoT tasks) {
    return tasks.containsCyclicDepWith(this);
  }
  
  boolean moreDependent(Task that) {
    if (that.prereqsContains(this.id)) {
      return false;
    }
    else if (this.prereqsLen() < that.prereqsLen()) {
      return false;
    }
    else return true;
  }
  
  boolean prereqsContains(int iD) {
    return this.prereqs.contains(iD);
  }
  
  int prereqsLen() {
    return this.prereqs.len();
  }
  
  boolean depends(Task that) {
    return this.prereqs.contains(that.id);
  }
  
  boolean cyclicDependency(Task that) {
    return this.depends(that) && that.depends(this);
  }
  
  boolean sameTask(Task that) {
    return this.id == that.id;
  }
  
}

interface ILoT {
  
  ILoT completeableTasks();
  ILoT completeableTasksHelp(Task depender, ILoT currentAns);
  boolean containsID(int iD);
  ILoT removeCyclics();
  boolean anyCyclics();
  boolean containsCyclicDepWith(Task that);
  ILoT removeTaskCyclicWith(Task that);
  boolean isSorted();
  boolean isSortedHelp(Task that);
  ILoT sortByDep();
  ILoT insertByDep(Task that);
  
}

class ConsT implements ILoT {
  Task first;
  ILoT rest;
  ConsT(Task first, ILoT rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public ILoT completeableTasks() {
    if (this.isSorted() && !this.anyCyclics()) {
      return this.rest.completeableTasksHelp(this.first, new MtLoT());
    }
    else if (!this.anyCyclics()) {
      return this.sortByDep().completeableTasks(); 
    }
    else if (this.isSorted()) {
      return this.removeCyclics().completeableTasks();
    }
    else 
      return this.sortByDep().removeCyclics().completeableTasks();
  }
  
  public ILoT completeableTasksHelp(Task depender, ILoT currentAns) {
    return this.rest.completeableTasksHelp(this.first, depender.findDependees(depender, this, currentAns));
  }
  
  public boolean containsID(int iD) {
    return this.first.id == iD || this.rest.containsID(iD);
  }
  
  public boolean anyCyclics() {
    return this.first.cyclicWithAnyIn(this.rest) || this.rest.anyCyclics();
  }
  
  public ILoT removeCyclics() {
    if (this.first.cyclicWithAnyIn(this.rest)) {
      return this.rest.removeTaskCyclicWith(this.first).removeCyclics();
    }
    else return new ConsT(this.first, this.rest.removeCyclics());
  }
  
  public ILoT removeTaskCyclicWith(Task that) {
    if (this.first.cyclicDependency(that)) {
      return this.rest;
    }
    else return new ConsT(this.first, this.rest.removeTaskCyclicWith(that));
  }
  
  public boolean containsCyclicDepWith(Task that) {
    return this.first.cyclicDependency(that) || this.rest.containsCyclicDepWith(that);
  }
  
  public boolean isSorted() {
    return this.rest.isSortedHelp(this.first) && this.rest.isSorted(); 
  }
  
  public boolean isSortedHelp(Task that) {
    if (this.first.cyclicDependency(that)) { return true; }
    else return that.moreDependent(this.first);
  }
  
  public ILoT sortByDep() {
    return this.rest.sortByDep().insertByDep(this.first);
  }
  
  public ILoT insertByDep(Task that) {
    if (this.first.moreDependent(that)) {
      return new ConsT(this.first, this.rest.insertByDep(that));
    }
    else return new ConsT(that, this);
  }

  

  
}

class MtLoT implements ILoT {
  MtLoT() {}
  
  public ILoT completeableTasks() {
    return this;
  }
  
  public ILoT completeableTasksHelp(Task depender, ILoT currentAns) {
    return depender.findDependees(depender, this, currentAns);
  }
  
  public boolean containsID(int iD) { return false; }
  
  public boolean anyCyclics() { return false; }
  
  public ILoT removeCyclics() { return this; }
  
  public boolean containsCyclicDepWith(Task that) { return false; }
  
  public ILoT removeTaskCyclicWith(Task that) { return this; }
  
  public boolean isSorted() {
    return true;
  }
  
  public boolean isSortedHelp(Task that) {
    return true;
  }
  
  public ILoT sortByDep() {
    return this;
  }
  
  public ILoT insertByDep(Task that) {
    return new ConsT(that, this);
  }

}

class ExampleTasks {
  
  Task task3 = new Task(3, new MtLoI());
  Task task4 = new Task(4, new MtLoI());
  Task task1 = new Task(1, new ConsLoI(3, new MtLoI()));
  Task task2 = new Task(2, new ConsLoI(4, new MtLoI()));
  Task task5 = new Task(5, new ConsLoI(6, new MtLoI()));
  Task badTask = new Task(6, new ConsLoI(1, new ConsLoI(2, new ConsLoI(5, new MtLoI()))));
  ConsT lot = new ConsT(task1, new ConsT(task3, new ConsT(task2, new ConsT(task5, new ConsT(task4, new ConsT(badTask, new MtLoT()))))));
  ILoT sortedLot = this.lot.sortByDep();
  
  boolean testSort(Tester t) {
    return
        t.checkExpect(lot.sortByDep(), 
            new ConsT(task5, 
                new ConsT(badTask, 
                    new ConsT(task2, new ConsT(task1, new ConsT(task4, new ConsT(task3, new MtLoT()))))))) &&
        t.checkExpect(sortedLot.isSorted(), true) &&
        t.checkExpect(this.lot.completeableTasks(), new ConsT(task3, new ConsT(task4, new ConsT(task1, new ConsT(task2, new MtLoT())))));
  }
}