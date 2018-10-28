# University of Washington, Programming Languages, Homework 6, hw6assignment.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  def initialize (point_array, board)
  	super
  end

  All_My_Pieces = All_Pieces 
  # All_Pieces += rotations([[0, 0], [0, -1], [1, 0]])] #
  

  # your enhancements here

end

class MyBoard < Board
  # your enhancements here

  def initialize (game)
  	super
  	@current_block = MyPiece.next_piece(self) # override since we've added pieces
  end


  def rotate180
  	if !game_over? and @game.is_running?
  		@current_block.move(0, 0, 2)
    end
    draw
  end

end

class MyTetris < Tetris
  # your enhancements here

  def key_bindings
  	super # keep all the previous key bindings from provided code in same method
  	
  	# call method rotate 180. need to proc b/c we need to call this key binding;
  	# can't call with a block b/c blocks are second-class
  	@root.bind('u', proc {board.rotate180}) 

  end

end