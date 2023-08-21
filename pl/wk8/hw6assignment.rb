# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.next_cheat_piece (board)
    MyPiece.new(Cheat_Piece.sample, board)
  end

  Cheat_Piece = [[[[0, 0]]]]
    
  All_My_Pieces = [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [-1, 0]]), #square w/ one extra
                   [[[0, 0], [-2, 0], [-1, 0], [1, 0], [2, 0]], # extra-long (only needs two)
                    [[0, 0], [0, -2], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [1, 0], [0, 1]]), # small L
                   [[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                   [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                   rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                   rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                   rotations([[0, 0], [1, 0], [0, -1], [-1, -1]])] # Z

end

class MyBoard < Board

  def intialize (game)
    super
    @current_block = MyPiece.next_piece(self)
    @cheat_block_switch = false
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    if @cheat_block_switch
    then @cheat_block_switch = false
      @current_block = MyPiece.next_cheat_piece(self)
    else @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(@current_block.current_rotation.size - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat_block
    if score >= 100 and not @cheat_block_switch
      @score = score - 100
      @cheat_block_switch = true
    end
  end
  
end

class MyTetris < Tetris

  def initialize
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    key_bindings
    buttons
    run_game
  end
  
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
    
  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat_block})
  end
  
end
