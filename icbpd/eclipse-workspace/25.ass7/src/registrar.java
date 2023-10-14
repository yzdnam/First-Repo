import tester.Tester;

class ExampleCourses {
  Instructor prof1, prof2;
  Course physics, chem, lit;
  Student stud1, stud2, stud3, stud4;
  
  void initRegistrar() {
    this.prof1 = new Instructor("Jamal", "Smart");
    this.prof2 = new Instructor("Pants", "Hollow");
  
    this.physics = new Course("Physics", 1, prof1);
    this.chem = new Course("Chemistry", 2, prof1);
    this.lit = new Course("Literature", 3, prof2);
  
    this.stud1 = new Student("Crew", "Neck");
    this.stud2 = new Student("Fart", "Sack");
    this.stud3 = new Student("Blue", "Collar");
    this.stud4 = new Student("New", "Dollar");
    
    this.stud1.addCourse(chem);
    this.stud1.addCourse(lit);
    this.stud2.addCourse(physics);
    this.stud3.addCourse(chem);
    this.stud3.addCourse(physics);
    this.stud4.addCourse(physics);
  }
  
  void testRegistrar(Tester t) {
    this.initRegistrar();
    // check Instructor
    t.checkExpect(this.prof1.courses, new ConsList<Course>(chem, new ConsList<Course>(physics, new MtList<Course>())));
    // check Course
    t.checkExpect(this.physics.enrollment, new ConsList<Student>(stud4, 
        new ConsList<Student>(stud3, new ConsList<Student>(stud2, new MtList<Student>()))));
  }
  
  void testIsEnrolled(Tester t) {
    this.initRegistrar();
    t.checkExpect(this.chem.isEnrolled(stud1), true);
    t.checkExpect(this.lit.isEnrolled(stud2), false);
  }
  
  void testClassmates(Tester t) {
    this.initRegistrar();
    t.checkExpect(this.stud1.classmates(stud3), true);
    t.checkExpect(this.stud1.classmates(stud2), false);
  }
  
  void testDejavu(Tester t) {
    this.initRegistrar();
    t.checkExpect(this.prof1.dejavu(stud3), true);
    t.checkExpect(this.prof1.dejavu(stud1), false);
  }
}

class Course {
  String name;
  int number;
  Instructor instruc;
  IList<Student> enrollment;
  Course(String name, int number, Instructor instruc) {
    this.name = name;
    this.number = number;
    this.instruc = instruc;
    this.enrollment = new MtList<Student>();
    this.instruc.updateCourses(this);
  }
  
  public void addStudent(Student newStud) {
    this.enrollment = new ConsList<Student>(newStud, this.enrollment);
  }
  
  public boolean isEnrolled(Student stud) {
    return this.enrollment.orMap(new SameStud(), stud);
  }
}

class Instructor {
  String first;
  String last;
  IList<Course> courses;
  Instructor(String first, String last) {
    this.first = first;
    this.last = last;
    this.courses = new MtList<Course>();
  }
  
  public boolean sameInstructor(Instructor other) {
    return first.equals(other.first) && last.equals(other.last);
  }
  
  public void updateCourses(Course newCourse) {
    if (!newCourse.instruc.sameInstructor(this)) {
      throw new RuntimeException("course is not taught by this instructor");
    }
    this.courses = new ConsList<Course>(newCourse, courses);
  }
  
  public boolean dejavu(Student c) {
    return this.courses.filter(new InClass(), c).length() > 1;
  }
}

class Student {
  String first;
  String last;
  IList<Course> courses;
  Student(String first, String last) {
    this.first = first;
    this.last = last;
    this.courses = new MtList<Course>();
  }
  
  public void addCourse(Course newCourse) {
    this.courses = new ConsList<Course>(newCourse, courses);
    newCourse.addStudent(this);
  }
  
  public boolean classmates(Student s) {
    return this.courses.orMap(new InClass(), s);
  }

}

class SameStud implements IPred2<Student> {
  public boolean apply(Student s1, Student s2) { return s1 == s2; }
}

class InClass implements IPred3<Course, Student> {
  public boolean apply(Course c, Student s) { return c.isEnrolled(s); }
}