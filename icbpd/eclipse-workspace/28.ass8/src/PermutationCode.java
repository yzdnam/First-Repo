import java.util.*;

import tester.Tester;

/**
 * A class that defines a new permutation code, as well as methods for encoding
 * and decoding of the messages that use this code.
 */

class PermExamples {
  PermutationCode testCode1, testCode2, testCode3;
  String encodedStr1, decodedStr1;
  
  void initCode() {
    ArrayList<Character> testCode1a =
        new ArrayList<Character>(Arrays.asList(
            'z', 'y', 'x', 'w', 'v', 'u', 't', 's', 'r', 'q',
            'p', 'o', 'n', 'm', 'l', 'k', 'j', 'i', 'h',
            'g', 'f', 'e', 'd', 'c', 'b', 'a'));
    testCode1 = new PermutationCode(testCode1a);
    encodedStr1 = "yovvw";
    decodedStr1 = "bleed";
    testCode2 = new PermutationCode();
    testCode3 = new PermutationCode();
  }
  
  void test(Tester t) {
    initCode();
    t.checkExpect(testCode1.decode(encodedStr1), "bleed");
    t.checkExpect(testCode1.encode(decodedStr1), encodedStr1);
    t.checkFail(testCode2.encode("abcdefghijklmnopqrstuvwxyz"), "abcdefghijklmnopqrstuvwxyz");
    t.checkFail(testCode2.encode(decodedStr1), testCode3.encode(decodedStr1));
  }
}
class PermutationCode {
    // The original list of characters to be encoded
    ArrayList<Character> alphabet = 
        new ArrayList<Character>(Arrays.asList(
                    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
                    'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 
                    't', 'u', 'v', 'w', 'x', 'y', 'z'));

    ArrayList<Character> code = new ArrayList<Character>(26);

    // A random number generator
    Random rand = new Random();

    // Create a new instance of the encoder/decoder with a new permutation code 
    PermutationCode() {
        this.code = this.initEncoder();
    }

    // Create a new instance of the encoder/decoder with the given code 
    PermutationCode(ArrayList<Character> code) {
        this.code = code;
    }

    // Initialize the encoding permutation of the characters
    ArrayList<Character> initEncoder() {
      ArrayList<Character> alphabetCopy =  new ArrayList<Character>(Arrays.asList(
          'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 
          'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 
          't', 'u', 'v', 'w', 'x', 'y', 'z'));
      for (int i = 26;
          i > 0;
          i = i - 1) {
        code.add(alphabetCopy.remove(rand.nextInt(i)));
      }
      return code;
    }

    // produce an encoded String from the given String
    String encode(String source) {
      return changeStr(alphabet, code, source);
    }
    
    // produce a decoded String from the given String
    String decode(String code) {
      return changeStr(this.code, alphabet, code);
    }
    
    String changeStr(ArrayList<Character> input, ArrayList<Character> output, String str) {
      int i;
      String ans = "";
        for (i = 0;
            i < str.length();
            i = i + 1) {
          ans = ans + new ChangeChar(input, output).apply(str.charAt(i));
        }
        return ans;
    }


}

interface IFunc<A, R> {
  R apply(A arg);
}

class ChangeChar implements IFunc<Character, Character> {
  ArrayList<Character> input;
  ArrayList<Character> output;
  ChangeChar(ArrayList<Character> input, ArrayList<Character> output) {
    this.input = input;
    this.output = output;
  }
  
  public Character apply(Character c) { return output.get(input.indexOf(c)); }
}
