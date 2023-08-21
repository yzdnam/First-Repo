## Solution template for Guess The Word practice problem (section 7)

require_relative './section-7-provided'

class ExtendedGuessTheWordGame < GuessTheWordGame
## YOUR CODE HERE
end

class ExtendedSecretWord < SecretWord

  def initialize word
    self.word = word
    @punc = ['!', '?', '.', ',', '(', ')', '-', ';', '\'', '"', ' ']
    self.pattern = make_pattern word
    @attempts = Array.new
  end

  def make_pattern iword
    if iword.length == 0
    then ''
    elsif @punc.any? {|mark| mark == iword[0]}
      iword[0] + (make_pattern (iword[1, (iword.length - 1)]))
    else '-' + (make_pattern (iword[1, (iword.length - 1)]))
    end
  end

  def valid_guess? guess
    guess.length == 1 and !@punc.any? {|mark| mark == guess} and !@attempts.any? {|attempt| attempt == guess}
  end

  def fill_pattern letter
    start = 0
    while ix = self.word.index(letter, start)
      self.pattern[ix] = self.word[ix]
      start = ix + 1
    end
  end

  def guess_letter! letter
    @attempts.push(letter)
    found = self.word.index letter
    if found
      fill_pattern letter
      if letter.upcase!
        @attempts.push(letter)
        fill_pattern letter
      elsif letter.downcase!
        @attempts.push(letter)
        fill_pattern letter
        end
    elsif letter.upcase!
      @attempts.push(letter)
      found = true
      fill_pattern letter
    elsif letter.downcase!
      @attempts.push(letter)
      found = true
      fill_pattern letter
    end
    found
  end

end

## Change to `false` to run the original game
if true
ExtendedGuessTheWordGame.new(ExtendedSecretWord).play
else
GuessTheWordGame.new(SecretWord).play
end
