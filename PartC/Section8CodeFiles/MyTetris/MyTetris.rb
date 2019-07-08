require_relative './MyTetrisGraphics'

# class responsible for the pieces and their movements
class Piece 
  
  # create a new Tetris piece from the given point array
  def initialize (point_array, board)
    @all_rotations = point_array
    @rotation_index = (0..(@all_rotations.size-1)).to_a.sample
    @color = All_Colors.sample
    @base_position = [5, 0] # [column, row]
    @board = board
    @moved = true
  end

  def current_rotation
    @all_rotations[@rotation_index]
  end
  
  def moved
    @moved
  end

  def position
    @base_position
  end

  def color
    @color
  end

  def drop_by_one
    @moved = move(0, 1, 0)
  end

  # take the intended movement in x, y and rotation and check to see if possible.  
  # If so, make movement.

  def move (delta_x, delta_y, delta_rotation)
    # Ensure rotation is a legitimate formation (as opposed 
    # to nil) by altering intended rotation so that it stays 
    # within bounds of rotation array
    moved = true
    potential = @all_rotations[(@rotation_index + delta_rotation) % @all_rotations.size]
    potential.each{|posns| 
      if !(@board.empty_at([posns[0] + delta_x + @base_position[0],
                            posns[1] + delta_y + @base_position[1]]));
        moved = false;  
      end
    }
    if moved
      @base_position[0] += delta_x
      @base_position[1] += delta_y
      @rotation_index = (@rotation_index + delta_rotation) % @all_rotations.size
    end
    moved
  end

  # class methods

  def self.rotations (point_array)
    rotate1 = point_array.map {|x,y| [-y,x]}  
    rotate2 = point_array.map {|x,y| [-x,-y]} 
    rotate3 = point_array.map {|x,y| [y,-x]}  
    [point_array, rotate1, rotate2, rotate3]  
  end

  def self.next_piece (board)
    Piece.new(All_Pieces.sample, board)
  end

  # class arrays
  
  All_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]])] # Z

  All_Colors = ['DarkGreen', 'dark blue', 'dark red', 'gold2', 'Purple3', 
               'OrangeRed2', 'LightSkyBlue']  
end


class Board

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = Piece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end
   
  def block_size
    15
  end
  
  def num_columns
    10
  end

  def num_rows
    27
  end
  
  # the current score
  def score
    @score
  end

  # the current delay
  def delay
    @delay
  end

  # game over when a piece is one row beyound top row
  def game_over?
    @grid[1].any?
  end

  def run
    ran = @current_block.drop_by_one
    if !ran
      store_current
      if !game_over?
        next_piece
      end
    end
    @game.update_score
    draw
  end

  def move_left
    if !game_over? and @game.is_running?
      @current_block.move(-1, 0, 0)
    end
    draw
  end

  def move_right
    if !game_over? and @game.is_running?
      @current_block.move(1, 0, 0)
    end
    draw
  end

  def rotate_clockwise
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 1)
    end
    draw
  end

  def rotate_counter_clockwise
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, -1)
    end
    draw
  end

  def rotate180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def down2
    if !game_over? and @game.is_running?
      @current_block.move(0, 2, 0)
    end
    draw
  end
  
  # drop the piece to lowest possible location in board, then get 
  # new piece and update score.
  def drop_all_the_way
    if @game.is_running?
      ran = @current_block.drop_by_one
      @current_pos.each{|block| block.remove}
      while ran
        @score += 1
        ran = @current_block.drop_by_one
      end
      draw
      store_current
      if !game_over?
        next_piece
      end
      @game.update_score
      draw
    end
  end

  def next_piece
    @current_block = Piece.next_piece(self)
    @current_pos = nil
  end

  # get information from piece on location on board, store piece on board,
  # then remove filled lines.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..3).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # check to see if a point on board is empty.
  def empty_at (point)
    if !(point[0] >= 0 and point[0] < num_columns)
      return false
    elsif point[1] < 1
      return true
    elsif point[1] >= num_rows
      return false
    end
    @grid[point[1]][point[0]] == nil
  end
 
  def remove_filled
    (2..(@grid.size-1)).each{|num| row = @grid.slice(num);
      # if row is full
      if @grid[num].all?
        # remove row
        (0..(num_columns-1)).each{|index|
          @grid[num][index].remove;
          @grid[num][index] = nil
        }
        # move down all higher rows
        ((@grid.size - num + 1)..(@grid.size)).each{|num2|
          @grid[@grid.size - num2].each{|rect| rect && rect.move(0, block_size)};
          @grid[@grid.size-num2+1] = Array.new(@grid[@grid.size - num2])
        }
        # insert new top blank row
        @grid[0] = Array.new(num_columns);
    
        @score += 10;
      end}
    self
  end

  def draw
    @current_pos = @game.draw_piece(@current_block, @current_pos)
  end
end

class Tetris

  # create and start game
  def initialize
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    key_bindings
    buttons
    @gameCounter = 1
    run_game
    @hiScore = 0
  end

  # create canvas
  def set_board
    @canvas = TetrisCanvas.new
    @board = Board.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings  
    @root.bind('n', proc {self.new_game}) 

    @root.bind('p', proc {self.pause}) 

    @root.bind('q', proc {exitProgram})
    
    @root.bind('a', proc {@board.move_left})
    @root.bind('Left', proc {@board.move_left}) 
    
    @root.bind('d', proc {@board.move_right})
    @root.bind('Right', proc {@board.move_right}) 

    @root.bind('s', proc {@board.down2})
    @root.bind('Down', proc {@board.down2})

    @root.bind('w', proc {@board.rotate_counter_clockwise})
    @root.bind('Up', proc {@board.rotate_counter_clockwise}) 
    
    @root.bind('space' , proc {@board.drop_all_the_way}) 

  end

  def buttons
    pauseTxt = TetrisButton.new('pause', 'lightcoral'){self.pause}
    pauseTxt.place(35, 50, 90, 7)

    new_game = TetrisButton.new('new game', 'lightcoral'){self.new_game}
    new_game.place(35, 75, 15, 7)
    
    quit = TetrisButton.new('quit', 'lightcoral'){exitProgram}
    quit.place(35, 50, 140, 7)
    
    move_left = TetrisButton.new('left', 'lightgreen'){@board.move_left}
    move_left.place(35, 50, 27, 536)
    
    move_right = TetrisButton.new('right', 'lightgreen'){@board.move_right}
    move_right.place(35, 50, 127, 536)
    
    rotate_clock = TetrisButton.new('^_)', 'lightgreen'){@board.down2}
    rotate_clock.place(35, 50, 77, 501)

    rotate_counter = TetrisButton.new('(_^', 'lightgreen'){
      @board.rotate_counter_clockwise}
    rotate_counter.place(35, 50, 77, 571)
    
    drop = TetrisButton.new('drop', 'lightgreen'){@board.drop_all_the_way}
    drop.place(35, 50, 77, 536)

    curScoreLabel = TetrisLabel.new(@root) do
      text 'Current: '   
      background 'lightblue'
    end
    curScoreLabel.place(35, 85, -10, 45) #[Font in Y, font in X, X (from left), Y (from top)]
    @scoreTxt = TetrisLabel.new(@root) do
      background 'lightblue'
    end
    @scoreTxt.text(@board.score)
    @scoreTxt.place(35, 30, 65, 45) 

    hiScoreLabel = TetrisLabel.new(@root) do
      text 'Hi: '   
      background 'lightblue'
    end
    hiScoreLabel.place(35, 85, 105, 45)
    @hiScoreTxt = TetrisLabel.new(@root) do
      background 'lightblue'
    end
    @hiScoreTxt.text('hullo')
    @hiScoreTxt.place(35, 30, 165, 45)  

  end

  # restart game, replacing the old board and score
  def new_game
    update_hiScore
    @canvas.unplace
    @canvas.delete
    set_board
    @gameCounter += 1
    @hiScoreTxt.text(@hiScore) # set the hi-score for each new game
    @scoreTxt.text(@board.score)
    @running = true
    run_game
  end

  # pause or resume game
  def pause
    if @running
      @running = false
      pauseTxt = TetrisButton.new('resume', 'lightcoral'){self.pause}
      pauseTxt.place(35, 50, 90, 7)
      @timer.stop
    else
      @running = true
      pauseTxt = TetrisButton.new('pause', 'lightcoral'){self.pause}
      pauseTxt.place(35, 50, 90, 7)
      self.run_game
    end
  end

  def update_score
    @scoreTxt.text(@board.score)
  end

  def update_hiScore
    if @board.score > @hiScore
      @hiScore = @board.score
    end
  end

  # repeatedly calls itself to fully automate game run.
  def run_game
    if !@board.game_over? and @running
      @timer.stop
      @timer.start(@board.delay, (proc{@board.run; run_game}))
    end
  end

  def is_running?
    @running
  end

  def gameCounter 
    @gameCounter
  end

  def draw_piece (piece, old=nil)
    if old != nil and piece.moved
      old.each{|block| block.remove}
    end
    size = @board.block_size
    blocks = piece.current_rotation
    start = piece.position
    blocks.map{|block| 
    TetrisRect.new(@canvas, start[0]*size + block[0]*size + 3, 
                       start[1]*size + block[1]*size,
                       start[0]*size + size + block[0]*size + 3, 
                       start[1]*size + size + block[1]*size, 
                       piece.color)}
  end
end

# for randomization of pieces
srand

class MyPiece < Piece

  All_My_Pieces = All_Pieces + 
    [rotations([[-3,0], [-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0], [3,0], [4,0]]), 
    rotations([[-1, 1], [0,1], [0, 0], [0, -1], [-1, -2], [1,-2], [0, -2]])]

  def self.next_piece (board)
  	MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheatPiece(board)
  	MyPiece.new([[[0,0]]], board)
  end

end

class MyBoard < Board
  def initialize (game) 
  	@grid = Array.new(num_rows) {Array.new(num_columns)}
    # updates current block to use "MyPiece"
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @curDelay = 500
    @increasedDelay = false
    @isJReward = false
    @eyesFlag = false
    @palFlag = false
    #initialize a boolean to determine whether current piece/move is cheating
    @isCheating = false
  end

  def curDelay
    @curDelay
  end

  def increaseDelay
    @curDelay = @delay
    if !@increasedDelay
      @delay = 1000
      @score -= 100
      @increasedDelay = true
    end
  end

  # flips piece while game is running
  def rotate180
  	if !game_over? and @game.is_running?
  		@current_block.move(0, 0, 2)
    end
    draw
  end

  def moveUp
    if !game_over? and @game.is_running?
      @current_block.move(0, -1, 0)
      @score -= 50
    end
    draw
  end

  def jReward
    if !@isJReward
      @score += 1000
      @isJReward = true
    end
  end

  def eyesFlag
    @eyesFlag
  end

  def palFlag
    @palFlag
  end

  def setEyes
    if !@eyesFlag
      @eyesFlag = true
    end
  end

  attr_accessor :palFlag, :score



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
  	if !@isCheating 
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
    if @increasedDelay
      @delay = @curDelay
      @increasedDelay = false
    end
    if @eyesFlag
      @palFlag = true
    end
  	@delay = [@delay - 4, 100].max
  end

end

class MyTetris < Tetris

  def key_bindings
  	super # keep all the previous key bindings from provided code in same method
  	
  	# call method rotate 180. need to proc b/c we need to call this key binding;
  	# can't call with a block b/c blocks are second-class
  	@root.bind('u', proc {@board.rotate180}) 

  	@root.bind('c', proc {@board.cheat})

    @root.bind('.', proc {@board.increaseDelay})

    @root.bind('/', proc {@board.moveUp})

    @root.bind('j', proc {@board.jReward})

    @root.bind(':', proc {@board.setEyes})

    @root.bind(')', proc {newBoard})

  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def newBoard
    if @board.eyesFlag && !@board.palFlag
      cur_Score = @board.score
      set_board
      @board.palFlag = true
      @board.score = cur_Score
    end
  end

end