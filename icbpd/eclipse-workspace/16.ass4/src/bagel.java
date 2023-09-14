import tester.*;

//TODO create function object for abs difference <= 0.001

class BagelRecipe {
  double flour;
  double water;
  double yeast;
  double malt;
  double salt;
  BagelRecipe(double flour, double water, double yeast, double malt, double salt) {
    if(Math.abs(flour - water) <= 0.001 && Math.abs(yeast - malt) <= 0.001 && Math.abs((salt + yeast) - (0.05 * flour)) <= 0.001) {
      this.flour = flour;
      this.water = water;
      this.yeast = yeast;
      this.malt = malt;
      this.salt = salt;
    }
    else {
      throw new IllegalArgumentException("Improper ingredient ratio given.");
    }
  }
  
  BagelRecipe(double flour, double yeast) {
    this(flour, flour, yeast, yeast, ((flour * 0.05) - yeast));
  }
  
  BagelRecipe(double flourCups, double yeastTsps, double saltTsps) {
    this(flourCups * 4.25, flourCups * 4.25, yeastTsps * 5 / 48, yeastTsps * 5 / 48, saltTsps * 5 / 24);
  }
  
  boolean sameRecipe(BagelRecipe other) {
    return Math.abs(other.flour - this.flour) <= 0.001 && Math.abs(other.water - this.water) <= 0.001 && 
        Math.abs(other.yeast - this.yeast) <= 0.001 && Math.abs(other.malt - this.malt) <= 0.001 &&
        Math.abs(other.salt - this.salt) <= 0.001;
  }
}

class ExampleRecipe {
  BagelRecipe r1 = new BagelRecipe(4.25,4.25,0.10625,0.10625,0.10625);
  BagelRecipe r2 = new BagelRecipe(4.25,0.10625);
  BagelRecipe r3 = new BagelRecipe(1.0,1.02,0.51);
  
  boolean testSameRec(Tester t) {
    return
        t.checkExpect(this.r1.sameRecipe(r2), true) &&
        t.checkExpect(this.r2.sameRecipe(r3), true);
  }
}