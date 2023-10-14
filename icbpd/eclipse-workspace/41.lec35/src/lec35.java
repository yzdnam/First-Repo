import java.util.ArrayList;
import tester.Tester;

class ExampleFib {
  Fib f = new Fib();
  
  void testFib(Tester t) {
    t.checkExpect(f.fib(5), 8);
  }
}
class Fib {
  Fib() {}

  int fib(int n) {
    ArrayList<Integer> answers = new ArrayList<Integer>();
    answers.add(1); // Base cases: fib(0) = 1
    answers.add(1); // fib(1) = 1
    fibAcc(n, answers);
    return answers.get(n);
  }
  int fibAcc(int n, ArrayList<Integer> answers) {
    // Check for redundant computation
    if (answers.size() > n) { return answers.get(n); }
    // Compute the new things:
    if (n == 0) { return 1; }
    else if (n == 1) { return 1; }
    else {
      int ans = this.fibAcc(n - 1, answers) + this.fibAcc(n - 2, answers);
      answers.add(ans);
      return ans;
    }
  }
  int fibIter(int n) {
    ArrayList<Integer> answers = new ArrayList<Integer>();
    answers.add(1);
    answers.add(1);
    for (int i = 2; i < n; i += 1) {
      answers.add(answers.get(i - 1) + answers.get(i - 2));
    }
    return answers.get(n);
  }
}

class ShoppingList {
  
  int bestScore(ArrayList<Integer> scores, ArrayList<Integer> costs, int remainingBudget) {
    ArrayList<ArrayList<Integer>> memos = new ArrayList<ArrayList<Integer>>();
    // It's a bit easier to pre-fill the array with placeholders,
    // than to try to dynamically fill it during the algorithm itself.
    for (int idx = 0; idx < scores.size(); idx += 1) {
      ArrayList<Integer> vals = new ArrayList<Integer>();
      for (int b = 0; b < remainingBudget; b += 1) {
        vals.add(Integer.MAX_VALUE); // Placeholder value to mark invalid answers
      }
      memos.add(vals);
    }
    bestScoreMemo(memos, scores, costs, 0, remainingBudget);
    return memos.get(0).get(remainingBudget);
  }
  int bestScoreMemo(ArrayList<ArrayList<Integer>> memos,
                    ArrayList<Integer> scores, ArrayList<Integer> costs,
                    int curItemIndex, int remainingBudget) {
    // Lookup memoized answer:
    if (memos.get(curItemIndex).get(remainingBudget) != Integer.MAX_VALUE) {
      return memos.get(curItemIndex).get(remainingBudget);
    }
    // Base case: no more items
    if (curItemIndex >= scores.size()) { return 0; }
    else {
      // Recursive case: take the better of...
      int ans = Math.max(
        // Try buying this item
        scores.get(curItemIndex) + bestScoreMemo(memos, scores, costs,
                                                curItemIndex + 1,
                                                remainingBudget - costs.get(curItemIndex)),
        // Skip buying this item
        bestScoreMemo(memos, scores, costs, curItemIndex + 1, remainingBudget)
        );
      memos.get(curItemIndex).set(remainingBudget, ans);
      return ans;
    }
  }
  int bestScoreIter(ArrayList<Integer> scores, ArrayList<Integer> costs, int remainingBudget) {
    ArrayList<ArrayList<Integer>> memos = new ArrayList<ArrayList<Integer>>();
    // It's a bit easier to pre-fill the array with placeholders,
    // than to try to dynamically fill it during the algorithm itself.
    for (int idx = 0; idx < scores.size(); idx += 1) {
      ArrayList<Integer> vals = new ArrayList<Integer>();
      for (int b = 0; b < remainingBudget; b += 1) {
        vals.add(Integer.MAX_VALUE); // Placeholder value to mark invalid answers
      }
      memos.add(vals);
    }
    for (int curItemIndex = 0; curItemIndex < scores.size(); curItemIndex += 1) {
      int firstIncludedScore = scores.get(curItemIndex);
      int firstIncludedBudget = remainingBudget - costs.get(curItemIndex);
      
      int firstExcludedScore = 0;
      int firstExcludedBudget = remainingBudget;
      
      for (int restItems = curItemIndex + 1; restItems < scores.size(); restItems += 1) {
        firstIncludedScore = firstIncludedScore + scores.get(restItems);
        firstIncludedBudget = firstIncludedBudget - costs.get(restItems);
        
        firstExcludedScore = firstExcludedScore + scores.get(restItems);
        firstExcludedBudget = firstExcludedBudget - costs.get(restItems);
      }
      int betterScore
    }
  }
}