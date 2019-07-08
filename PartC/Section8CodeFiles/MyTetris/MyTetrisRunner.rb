# University of Washington, Programming Languages, Homework 6, hw6runner.rb

require_relative './MyTetris'
require_relative './MyTetrisGraphics'

def runMyTetris
  MyTetris.new
  mainLoop
end

runMyTetris