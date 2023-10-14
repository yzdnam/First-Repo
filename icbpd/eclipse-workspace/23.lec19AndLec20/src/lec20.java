    // Represents a sentinel at the start, a Vertex in the middle,
    // or the empty end of a list
    abstract class APersonList {
      abstract void removePersonHelp(String name, AVertex prev);
      APersonList() { } // nothing to do here
    }

    // Represents a Vertex in a list that has some list after it
    abstract class AVertex extends APersonList {
      APersonList rest;
      AVertex(APersonList rest) {
        this.rest = rest;
      }
      
      public abstract void removePersonHelp(String name, AVertex prev);
    }

    // Represents the empty end of the list
    class MtLoPerson extends APersonList {
      MtLoPerson() { } // nothing to do
      void removePersonHelp(String name, AVertex prev) { return; }
    }

    // Represents a data Vertex in the list
    class ConsLoPerson extends AVertex {
      Person data;
      ConsLoPerson(Person data, APersonList rest) {
        super(rest);
        this.data = data;
      }
      public void removePersonHelp(String name, AVertex prev) {
        if (this.data.name.equals(name)) {
          prev.rest = this.rest;
        }
        else {
          this.rest.removePersonHelp(name, this);
        }
      }
    }


 // Represents the dummy Vertex before the first actual Vertex of the list
    class Sentinel extends AVertex {
      Sentinel(APersonList rest) {
        super(rest);
      }
      public void removePersonHelp(String name, AVertex prev) {
        throw new RuntimeException("Can't try to remove on a Sentinel!");
      }
    }
    
    class MutablePersonList {
      Sentinel sentinel;
      MutablePersonList() {
        this.sentinel = new Sentinel(new MtLoPerson());
      }
      
   // In MutablePersonList
      void removePerson(String name) {
        this.sentinel.rest.removePersonHelp(name, this.sentinel);
      }
      
      void addPerson(String name, int num) {
        this.sentinel.rest = new ConsLoPerson(new Person(name, num), this.sentinel.rest);
      }
    }
    
    