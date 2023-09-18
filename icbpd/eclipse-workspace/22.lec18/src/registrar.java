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
  }
  
  void testRegistrar(Tester t) {
    this.initRegistrar();
    // check initial conditions
    t.checkExpect(this.physics.enrollment, new MtList<Student>());
    // modify initial conditions
    this.stud1.addCourse(chem);
    this.stud1.addCourse(lit);
    this.stud2.addCourse(physics);
    this.stud3.addCourse(chem);
    this.stud4.addCourse(physics);
    // check Instructor
    t.checkExpect(this.prof1.courses, new ConsList<Course>(chem, new ConsList<Course>(physics, new MtList<Course>())));
    // check Course
    t.checkExpect(this.physics.enrollment, new ConsList<Student>(stud4, 
        new ConsList<Student>(stud2, new MtList<Student>())));
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
}