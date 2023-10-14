import java.util.*;
import tester.*;

class ExampleCurric {
  Curriculum curr;
  ArrayList<Course> exampleCourses;
  Course fund1, fund2, dbd, algos, systems, ood, theory, lspdp, pl, compilers;
  
  void initCurric() {

    fund1 = new Course("Fundamentals 1");
    fund2 = new Course("Fundamentals 2");
    fund2.addPrereq(fund1);
    dbd = new Course("Database Design");
    dbd.addPrereq(fund1);
    algos = new Course("Algorithms and Data");
    algos.addPrereq(fund2);
    systems = new Course("Computer Systems");
    systems.addPrereq(fund2);
    ood = new Course("Object-Oriented Design");
    ood.addPrereq(fund2);
    theory = new Course("Theory of Computation");
    theory.addPrereq(fund2);
    lspdp = new Course("Large-Scale Parallel Data Processing");
    lspdp.addPrereq(algos);
    lspdp.addPrereq(systems);
    pl = new Course("Programming Languages");
    pl.addPrereq(ood);
    pl.addPrereq(theory);
    compilers = new Course("Compilers");
    compilers.addPrereq(pl);
    
    exampleCourses = new ArrayList<Course>();
    exampleCourses.add(fund1);
    exampleCourses.add(dbd);
    exampleCourses.add(fund2);
    exampleCourses.add(algos);
    exampleCourses.add(systems);
    exampleCourses.add(ood);
    exampleCourses.add(theory);
    exampleCourses.add(lspdp);
    exampleCourses.add(pl);
    exampleCourses.add(compilers);
    
    curr = new Curriculum();
    curr.addCourse(algos);
    curr.addCourse(compilers);
    curr.addCourse(systems);
    curr.addCourse(dbd);
    curr.addCourse(fund1);
    curr.addCourse(fund2);
    curr.addCourse(lspdp);
    curr.addCourse(ood);
    curr.addCourse(pl);
    curr.addCourse(theory);
  }
  
  void testComesBefore(Tester t) {
    initCurric();
    t.checkExpect(curr.comesAfterPrereqs(exampleCourses, compilers), false);
    t.checkExpect(curr.comesAfterPrereqs(exampleCourses, fund1), true);
    t.checkExpect(curr.validSchedule(exampleCourses), false);
    curr.courses = curr.topSort(curr.courses);
    t.checkExpect(curr.validSchedule(curr.courses), true);
  }
}

class Curriculum {
  ArrayList<Course> courses;
  Curriculum() { this.courses = new ArrayList<Course>(); }
  // EFFECT: adds another course to the set of known courses
  void addCourse(Course c) { this.courses.add(c); }
  // add methods here...
  
  boolean comesAfterPrereqs(ArrayList<Course> schedule, Course c) {
    ArrayList<Integer> prereqIdxs = new ArrayList<Integer>();
    for (Course prereq : c.prereqs) {
      prereqIdxs.add(this.courses.indexOf(prereq));
    }
    for (Integer i : prereqIdxs) {
      if (i > this.courses.indexOf(c)) {
        return false;
      }
    }
    return true;
  }
  
  boolean validSchedule(ArrayList<Course> schedule) {
    for (Course c : schedule) {
      if (!this.comesAfterPrereqs(schedule, c)) {
        return false;
      }
    }
    return true;
  }
  
  ArrayList<Course> topSort(ArrayList<Course> unsortedCourses) {
    ArrayList<Course> result = new ArrayList<Course>();
    for (Course c : unsortedCourses) {
      c.process(result);
    }
    return result;
  }
}
class Course {
  String name;
  ArrayList<Course> prereqs;
  Course(String name) { this.name = name; this.prereqs = new ArrayList<Course>(); }
  // EFFECT: adds a course as a prereq to this one
  void addPrereq(Course c) { this.prereqs.add(c); }
  // add methods here
  
  void process(ArrayList<Course> processed) {
    if (!processed.contains(this)) {
      for (Course c : this.prereqs) {
        c.process(processed);
      }
      processed.add(this);
    }
  }
}