import tester.*;

class EmbroideryPiece {
  String name;
  Motif motif;
  EmbroideryPiece(String name, Motif motif) {
    this.name = name;
    this.motif = motif;
  }
  
  double averageDifficulty() {
    return motif.averageDifficulty();
  }
  
  String embroideryInfo() {
    return this.name + ": " + this.motif.printInfo();
  }
}

interface Motif {
  double averageDifficulty();
  int count();
  double totalDifficulty();
  String printInfo();
}

class CrossStitchMotif implements Motif {
  String description;
  Double difficulty;
  CrossStitchMotif(String description, Double difficulty) {
    this.description = description;
    this.difficulty = difficulty;
  }
  
  public double averageDifficulty() {
    return this.difficulty;
  }
  
  public double totalDifficulty() {
    return this.difficulty;
  }
  
  public int count() { return 1; }
 
  public String printInfo() {
    return this.description + " (cross stitch)";
  }
  
}

class ChainStitchMotif implements Motif {
  String description;
  Double difficulty;
  ChainStitchMotif(String description, Double difficulty) {
    this.description = description;
    this.difficulty = difficulty;
  }
  
  public double averageDifficulty() {
    return this.difficulty;
  }
  
  public double totalDifficulty() {
    return this.difficulty;
  }
  
  public int count() { return 1; }

  public String printInfo() {
    return this.description + " (chain stitch)";
  }
  
}

class GroupMotif implements Motif {
  String description;
  ILoMotifs motifs;
  GroupMotif(String description, ILoMotifs motifs) {
    this.description = description;
    this.motifs = motifs;
  }
  
  public double averageDifficulty() {
    return this.motifs.averageDifficulty();
  }
  
  public double totalDifficulty() {
    return this.motifs.totalDifficulty();
  }
  
  public int count() {
    return this.motifs.count();
  }
  
  public String printInfo() {
    return this.motifs.printInfo(this.count());
  }
 
}

interface ILoMotifs {
  double averageDifficulty();
  double totalDifficulty();
  int count();
  String printInfo(int cnt);
  String printHelp(int cnt);
}

class ConsLoMotifs implements ILoMotifs {
  Motif first;
  ILoMotifs rest;
  ConsLoMotifs(Motif first, ILoMotifs rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public double averageDifficulty() {
    return this.totalDifficulty() / this.count();
  }
  
  public double totalDifficulty() {
    return this.first.totalDifficulty() + this.rest.totalDifficulty();
  }
  
  public int count() {
    return this.first.count() + this.rest.count();
  }
  
  public String printInfo(int cnt) {
    return this.first.printInfo() + this.rest.printHelp(cnt - 1);
  }
  
  public String printHelp(int cnt) {
    return ", " + this.first.printInfo() + ", " + this.rest.printInfo(cnt - 1);
  }
}

class MtLoMotifs implements ILoMotifs {
  MtLoMotifs() {};
  
  public double averageDifficulty() { return 0.0; }
  
  public double totalDifficulty() { return 0.0; }
  
  public int count() { return 0; }
  
  public String printInfo(int cnt) { return ""; }
  
  public String printHelp(int cnt) {
    if (cnt <= 0) { return "."; }
    else { return ""; }
  }
}

class ExamplesEmbroidery {
  
  MtLoMotifs mt = new MtLoMotifs();
  
  CrossStitchMotif bird = new CrossStitchMotif("bird",4.5);
  ChainStitchMotif tree = new ChainStitchMotif("tree",3.0);
  CrossStitchMotif rose = new CrossStitchMotif("rose",5.0);
  ChainStitchMotif poppy = new ChainStitchMotif("poppy",4.75);
  CrossStitchMotif daisy = new CrossStitchMotif("daisy",3.2);
  GroupMotif flowers = new GroupMotif("flowers", new ConsLoMotifs(rose, new ConsLoMotifs(poppy, new ConsLoMotifs(daisy, mt))));
  GroupMotif nature = new GroupMotif("nature", new ConsLoMotifs(bird, new ConsLoMotifs(tree, new ConsLoMotifs(flowers, mt))));
  
  EmbroideryPiece pillowCover = new EmbroideryPiece("Pillow Cover", nature);
  
  boolean testAvgDiff(Tester t) {
    return
        t.checkExpect(this.flowers.motifs.totalDifficulty(), 12.95) &&
        t.checkExpect(this.nature.motifs.totalDifficulty(), 20.45) &&
        t.checkExpect(this.nature.count(), 5) &&
        t.checkInexact(this.flowers.averageDifficulty(), 4.3166, 0.001) &&
        t.checkInexact(this.pillowCover.averageDifficulty(), 4.09, 0.001) &&
        t.checkExpect(this.pillowCover.embroideryInfo(), 
            "Pillow Cover: bird (cross stitch), tree (chain stitch), rose (cross stitch), poppy (chain stitch), daisy (cross stitch).");
  }
  
}