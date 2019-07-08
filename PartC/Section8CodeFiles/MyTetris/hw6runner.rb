# University of Washington, Programming Languages, Homework 6, hw6runner.rb

require_relative './hw6provided'
require_relative './hw6assignment'

def runTetris
  Tetris.new 
  mainLoop
end

def runMyTetris
  MyTetris.new
  mainLoop
end

runMyTetris