import org.junit.Test;
import tester.Tester;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;

public class ExamplesShapesInJUnit {
    public ExamplesShapesInJUnit() {}
    
    CartPt pt1 = new CartPt(0, 0);
    CartPt pt2 = new CartPt(3, 4);
    CartPt pt3 = new CartPt(7, 1);
    
    IShape c1 = new Circle(new CartPt(50, 50), 10, "red");
    IShape c2 = new Circle(new CartPt(50, 50), 30, "red");
    IShape c3 = new Circle(new CartPt(30, 100), 30, "blue");
    
    IShape s1 = new Square(new CartPt(50, 50), 30, "red");
    IShape s2 = new Square(new CartPt(50, 50), 50, "red");
    IShape s3 = new Square(new CartPt(20, 40), 10, "green");
    
    // test the method distToOrigin in the class CartPt
    @Test
    public void testDistToOrigin() {
        assertEquals(0.0, this.pt1.distToOrigin(), 0.001);
        assertEquals(5.0,this.pt2.distToOrigin(), 0.001);
    }
    //TODO convert all tests to JUnit tests
    // test the method distTo in the class CartPt
    @Test
    public void testDistTo() { 
        
        assertEquals(5.0, this.pt1.distTo(this.pt2), 0.001);
        assertEquals(5.0, this.pt2.distTo(this.pt3), 0.001);
    }
    
    // test the method area in the class Circle
    @Test
    public void testCircleArea() { 
      assertEquals(314.15, this.c1.area(), 0.01);
    }
    
    // test the method grow in the class Circle
    boolean testSquareArea(Tester t) { 
        return
        t.checkInexact(this.s1.area(), 900.0, 0.01);
    }
    
    // test the method distToOrigin in the class Circle
    boolean testCircleDistToOrigin(Tester t) { 
        return
        t.checkInexact(this.c1.distToOrigin(), 60.71, 0.01) &&
        t.checkInexact(this.c3.distToOrigin(), 74.40, 0.01);
    }
    
    // test the method distTo in the class Circle
    boolean testSquareDistToOrigin(Tester t) { 
        return
        t.checkInexact(this.s1.distToOrigin(), 70.71, 0.01) &&
        t.checkInexact(this.s3.distToOrigin(), 44.72, 0.01);
    }
    
    // test the method grow in the class Circle
    @Test
    public void testCircleGrow() { 
        assertEquals(this.c1.grow(20), this.c2);
    }
    
    // test the method grow in the class Circle
    boolean testSquareGrow(Tester t) { 
        return
        t.checkExpect(this.s1.grow(20), this.s2);
    }
    
    // test the method biggerThan in the class Circle
    boolean testCircleBiggerThan(Tester t) { 
        return
        t.checkExpect(this.c1.biggerThan(this.c2), false) && 
        t.checkExpect(this.c2.biggerThan(this.c1), true) && 
        t.checkExpect(this.c1.biggerThan(this.s1), false) && 
        t.checkExpect(this.c1.biggerThan(this.s3), true);
    }
    
    // test the method biggerThan in the class Square
    boolean testSquareBiggerThan(Tester t) { 
        return
        t.checkExpect(this.s1.biggerThan(this.s2), false) && 
        t.checkExpect(this.s2.biggerThan(this.s1), true) && 
        t.checkExpect(this.s1.biggerThan(this.c1), true) && 
        t.checkExpect(this.s3.biggerThan(this.c1), false);
    }
    
    // test the method contains in the class Circle
    @Test
    public void testCircleContains() { 
      assertFalse("The contains method for circle does not work properly",this.c1.contains(new CartPt(100,100)));
      assertTrue(this.c2.contains(new CartPt(40,60)));
    }
    
    
    // test the method contains in the class Square
    boolean testSquareContains(Tester t) { 
        return
        t.checkExpect(this.s1.contains(new CartPt(100, 100)), false) && 
        t.checkExpect(this.s2.contains(new CartPt(55, 60)), true);
    }
}