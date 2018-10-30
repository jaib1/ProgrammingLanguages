# University of Washington, Programming Languages, Homework 6, hw6assignment.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # update "All_Pieces" to include 3 new pieces
  All_My_Pieces = All_Pieces + [rotations([[0, 1], [0, 0], [1, 0]]),
  rotations([[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]]),
  rotations([[-1, 1], [0, 1], [-1,0], [0, 0], [1, 0]])]


  # your enhancements here
  def self.next_piece (board)
  	# update "self.next_piece" to take into account "All_My_Pieces"
  	MyPiece.new(All_My_Pieces.sample, board)
  end

  # add a cheat piece
  def self.cheatPiece(board)
  	# has to be triple nested in array b/c all other pieces in "All_My_Pieces" are
  	MyPiece.new([[[0,0]]], board)
  end

end

class MyBoard < Board
  # your enhancements here

  def initialize (game) 
  	@grid = Array.new(num_rows) {Array.new(num_columns)}
    # updates current block to use "MyPiece"
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500

    #initialize a boolean to determine whether current piece/move is cheating
    @isCheating = false
  end

  # flips piece while game is running
  def rotate180
  	if !game_over? and @game.is_running?
  		@current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
  	# update "next_piece" to use cheat piece when called (via 'c')
  	if @isCheating
  	  @current_block = MyPiece.cheatPiece(self)
  	  @isCheating = false
  	else
  	  # update "next_piece" to pull from 'MyPiece'
  	  @current_block = MyPiece.next_piece(self)
  	end
  	  @current_pos = nil
  end
  
  # cheating gonna cost ya...(- 100)
  def cheat
  	# make sure we only penalize cheating once per turn
  	if @score >= 100 && !@isCheating 
  	  @score -= 100
  	  @isCheating = true
  	end
  end

  def store_current
  	locations = @current_block.current_rotation
  	displacement = @current_block.position
  	# for each unit in the block, update the grid
  	(0..locations.size-1).each{|index| 
  		current = locations[index];
  		@grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
  		@current_pos[index]
  	}
  	remove_filled
  	@delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  # your enhancements here

  def key_bindings
  	super # keep all the previous key bindings from provided code in same method
  	
  	# call method rotate 180. need to proc b/c we need to call this key binding;
  	# can't call with a block b/c blocks are second-class
  	@root.bind('u', proc {@board.rotate180}) 

  	@root.bind('c', proc {@board.cheat})

  end

  def set_board
  	super
    @board = MyBoard.new(self)
  end

end