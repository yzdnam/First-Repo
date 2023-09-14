import tester.*;

interface Document {

  // recursively scans all bibliographies down from the given document's bibliography and consolidates them into a single bibliography
  Bibliography consolidateBiblios();
  // removes all Wiki entries from a document's bibliography
  Bibliography removeWikis();
  Bibliography removeWikiEntry(Bibliography bib);
  
  Document sortBiblio();
  boolean compareAuthors(Document doc);
  boolean compareAuthorHelp(String auth);
  
  String printBiblio();
  String printEntry();
  String printBiblioRecurs();
  String printProperBiblio();
  
  boolean sameDoc(Document doc);
  boolean sameDocAuthorHelp(String other_auth);
  boolean sameDocTitleHelp(String other_title);
  
}

class Book implements Document {
  String author;
  String title;
  Bibliography bibliography;
  String publisher;
  Book(String author, String title, Bibliography bibliography, String publisher) {
    this.author = author;
    this.title = title;
    this.bibliography = bibliography;
    this.publisher = publisher;
  }
  
  public Bibliography consolidateBiblios() {
    return this.bibliography.consolidateBiblios();
  }
  
  public Bibliography removeWikis() {
    return this.bibliography.removeWikis();
  }
  
  public Bibliography removeWikiEntry(Bibliography bib) {
    return new ConsBiblio(this, bib.removeWikis());
  }
  
  public Document sortBiblio() {
    return new Book(this.author, this.title, this.bibliography.sortBiblio(), this.publisher);
  }
  
  public boolean compareAuthors(Document doc) {
    return doc.compareAuthorHelp(this.author);
  }

  // returns true if this document's author precedes the argument
  public boolean compareAuthorHelp(String auth) {
    return this.author.compareTo(auth) > 0;
  }
  
  public String printBiblio() {
    return this.bibliography.printBiblio(this.bibliography.countEntries());
  }
  
  public String printBiblioRecurs() {
    return this.bibliography.printBiblio(this.bibliography.countEntries() + 1);
  }
  
  public String printEntry() {
    return this.author + ". " + this.title;
  }
  
  public boolean sameDoc(Document doc) {
    return doc.sameDocAuthorHelp(this.author) && doc.sameDocTitleHelp(this.title);
  }
  
  public boolean sameDocAuthorHelp(String other_auth) {
    return this.author.compareTo(other_auth) == 0;
  }
  
  public boolean sameDocTitleHelp(String other_title) {
    return this.title.compareTo(other_title) == 0;
  }
  
  public String printProperBiblio() {
    Bibliography noWikis = this.consolidateBiblios().sortBiblio().removeWikis().removeDupes();
    return noWikis.printBiblio(noWikis.countEntries());
  }

}

class WikiArticle implements Document {
  String author;
  String title;
  Bibliography bibliography;
  String url;
  WikiArticle(String author, String title, Bibliography bibliography, String url) {
    this.author = author;
    this.title = title;
    this.bibliography = bibliography;
    this.url = url;
  }
  
  public Bibliography consolidateBiblios() {
    return this.bibliography.consolidateBiblios();
  }
  
  public Bibliography removeWikis() {
    return this.bibliography.removeWikis();
  }
  
  public Bibliography removeWikiEntry(Bibliography bib) {
    return bib.removeWikis();
  }
  
  public Document sortBiblio() {
    return new WikiArticle(this.author, this.title, this.bibliography.sortBiblio(), this.url);
  }
  
  public boolean compareAuthors(Document doc) {
    return doc.compareAuthorHelp(this.author);
  }

  // returns true if this document's author precedes the argument
  public boolean compareAuthorHelp(String auth) {
    return this.author.compareTo(auth) > 0;
  }
  
  public String printBiblio() {
    return this.bibliography.printBiblio(this.bibliography.countEntries());
  }
  
  public String printBiblioRecurs() {
    return this.bibliography.printBiblio(this.bibliography.countEntries() + 1);
  }
  
  public String printEntry() {
    return this.author + ". " + this.title;
  }
  
  public boolean sameDoc(Document doc) {
    return doc.sameDocAuthorHelp(this.author) && doc.sameDocTitleHelp(this.title);
  }
  
  public boolean sameDocAuthorHelp(String other_auth) {
    return this.author.compareTo(other_auth) == 0;
  }
  
  public boolean sameDocTitleHelp(String other_title) {
    return this.title.compareTo(other_title) == 0;
  }
  
  public String printProperBiblio() {
    Bibliography noWikis = this.consolidateBiblios().sortBiblio().removeWikis().removeDupes();
    return noWikis.printBiblio(noWikis.countEntries());
  }
  
}

interface Bibliography {
  
  int countEntries();
  Bibliography appendBiblios(Bibliography other);
  Bibliography consolidateBiblios();
  Bibliography removeWikis();
  
  Bibliography sortBiblio();
  Bibliography insert(Document doc);
  
  String printBiblio(int cnt);
  String printBiblioHelp(int cnt);
  
  boolean contains(Document doc);
  Bibliography removeDupes();
  
}

class MtBiblio implements Bibliography {
  MtBiblio() {};
  
  public int countEntries() {
    return 0;
  }
  
  public Bibliography appendBiblios(Bibliography other) {
    return other;
  }
  
  public Bibliography consolidateBiblios() {
    return this;
  }
  
  public Bibliography removeWikis() {
    return this;
  }
  
  public Bibliography sortBiblio() {
    return this;
  }
  
  public Bibliography insert(Document doc) {
    return new ConsBiblio(doc, this);
  }
  
  public boolean contains(Document doc) {
    return false;
  }
  
  public Bibliography removeDupes() {
    return this;
  }
  
  public String printBiblio(int cnt) {
    return ""; 
  }
  
  public String printBiblioHelp(int cnt) {
    if (cnt < 1) { return ""; }
    else { return "\n"; }
  }
  
}

class ConsBiblio implements Bibliography {
  Document first;
  Bibliography rest;
  ConsBiblio(Document first, Bibliography rest) {
    this.first = first;
    this.rest = rest;
  }
  
  public int countEntries() {
    return 1 + this.rest.countEntries();
  }
  
  public Bibliography appendBiblios(Bibliography other) {
    return new ConsBiblio(this.first, this.rest.appendBiblios(other));
  }
  
  public Bibliography consolidateBiblios() {
    return new ConsBiblio(this.first, this.first.consolidateBiblios()).appendBiblios(this.rest.consolidateBiblios()); 
  }
  
  public Bibliography removeWikis() {
    return this.first.removeWikiEntry(this.rest);
  }
  
  public Bibliography sortBiblio() {
    return this.rest.sortBiblio().insert(this.first);
  }
  
  public Bibliography insert(Document doc) {
    if (doc.compareAuthors(this.first)) {
      return new ConsBiblio(doc, this);
    }
    else {
      return new ConsBiblio(this.first, this.rest.insert(doc));
    }
  }
  
  public boolean contains(Document doc) {
    if (this.first.sameDoc(doc)) { return true; }
    else return this.rest.contains(doc);
  }
  
  public Bibliography removeDupes() {
    if (this.rest.contains(this.first)) {
      return this.rest.removeDupes();
    }
    else return new ConsBiblio(this.first, this.rest.removeDupes());
  }
  
  public String printBiblio(int cnt) {
    return this.first.printEntry() + this.rest.printBiblioHelp(cnt - 1);
  }
  
  public String printBiblioHelp(int cnt) {
    if (cnt == 1) { 
      return "\n" + this.first.printEntry();
    }
    else {
      return "\n" + this.first.printEntry() + "\n" + this.rest.printBiblio(cnt - 1);
    }
  }
  
}

class Prob1Examples {
  
  MtBiblio end = new MtBiblio();
  Book book1 = new Book("attack, mack", "title1", end, "Random House");
  ConsBiblio biblio1 = new ConsBiblio(book1, end);
  Book book2 = new Book("box, fart", "title2", biblio1, "nutsack");
  
  Book book3 = new Book("shot, blarb", "title3.1", end, "Random House");
  ConsBiblio biblio2 = new ConsBiblio(book3, new ConsBiblio(book2, end));
  WikiArticle wiki1 = new WikiArticle("chuck, barb", "title3", biblio2, "www.wiki.com");
  
  ConsBiblio biblio3 = new ConsBiblio(wiki1, new ConsBiblio(book2, new ConsBiblio(book1, end)));
  Book mypaper = new Book("fun, cake", "title4", biblio3, "pubstack");
  
  boolean testBooksOnly(Tester t) {
    return
        t.checkExpect(this.biblio2.countEntries(), 2) &&
        t.checkExpect(this.wiki1.consolidateBiblios(), new ConsBiblio(book3, new ConsBiblio(book2, new ConsBiblio(book1, end)))) &&
        t.checkExpect(this.wiki1.printBiblio(), "shot, blarb. title3.1\nbox, fart. title2") &&
        t.checkExpect(this.mypaper.sameDoc(book1), false) &&
        t.checkExpect(this.mypaper.sameDoc(this.mypaper), true) &&
        t.checkExpect(this.mypaper.printProperBiblio(), "");
  }
  
}
