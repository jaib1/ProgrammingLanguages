# University of Washington, Programming Languages, Homework 7, hw7.rb 
# (See also ML code)

# a little language for 2D geometry objects

# each subclass of GeometryExpression, including subclasses of GeometryValue,
#  needs to respond to messages preprocess_prog and eval_prog
#
# each subclass of GeometryValue additionally needs:
#   * shift
#   * intersect, which uses the double-dispatch pattern
#   * intersectPoint, intersectLine, and intersectVerticalLine for 
#       being called by intersect of appropriate clases and doing
#       the correct intersection calculuation
#   * (We would need intersectNoPoints and intersectLineSegment, but these
#      are provided by GeometryValue and should not be overridden.)
#   *  intersectWithSegmentAsLineResult, which is used by 
#      intersectLineSegment as described in the assignment
#
# you can define other helper methods, but will not find much need to

# Note: geometry objects should be immutable: assign to fields only during
#       object construction

# Note: For eval_prog, represent environments as arrays of 2-element arrays
# as described in the assignment

class GeometryExpression  
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue 
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2) 
    (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2) 
    real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2) 
    if real_close(x1,x2)
      VerticalLine.new x1
    else
      m = (y2 - y1).to_f / (x2 - x1)
      b = y1 - m * x1
      Line.new(m,b)
    end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    #line_result.intersectWithSegmentAsLineResult seg
  end

  def eval_prog env 
    self # all values evaluate to self
  end

  def preprocess_prog
    self # no pre-processing to do here
  end

  def shift(dx,dy)
    self # shifting no-points is no-points
  end

  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end

  def intersectNoPoints x
    NoPoints.new
  end

  def intersectPoint p
    self # intersection with point and no-points is no-points
  end

  def intersectLine line
    self # intersection with line and no-points is no-points
  end

  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end

  # if self is the intersection of (1) some shape s and (2) 
  # the line containing seg, then we return the intersection of the 
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end

end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)
  # However, you *may* move methods from here to a superclass if you wish to

  # Note: no initialize method only because there is nothing it needs to do

  #inherits everything (doesn't do anything)
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y

  def initialize(x,y)
    @x = x
    @y = y
  end

  def shift(dx,dy)
    Point.new(@x+dx, @y+dy)
  end

  def intersect other # including NoPoints
    other.intersectPoint self
  end

  def intersectPoint p
    if real_close_point(@x, @y, p.x, p.y)
      self
    else
      NoPoints.new
    end
  end

  def intersectLine line
    if real_close(@y, (line.m * @x + line.b))
      self
    else
      NoPoints.new
    end
  end

  def intersectVerticalLine vLine
    if real_close(@x, vLine.x)
      self
    else
      NoPoints.new
    end
  end

  def intersectWithSegmentasLineResult seg
    if inbetween(@x, seg.x1, seg.x2) && inbetween(@y, seg.y1, seg.y2)
      p
    else
      NoPoints.new
    end
  end

  # helper function for point with segment case. needs to be made private
  # so (only) the instantiated object of this class can call it
  private

  def inbetween(px, end1x, end2x)
    ((end1x - GeometryExpression::Epsilon) <= px) && \
      (px <= (end2x + GeometryExpression::Epsilon)) \
      or \
      ((end2x - GeometryExpression::Epsilon) <= px) && \
      (px <= (end1x + GeometryExpression::Epsilon))
  end

  # inherits preprocess_prog, eval_prog
  # (which doesn't do anything, and just returns the object instantiation

end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b 

  def initialize(m,b)
    @m = m
    @b = b
  end

  def shift(dx,dy)
    Line.new(@m, ((@b+dy) - (@m*dx)))
  end

  def intersect other 
    other.intersectLine self
  end

  def intersectPoint p
    p.intersectLine(self) # we can do this to re-use the Point case
  end

  def intersectLine line
    if real_close(@m, line.m)
      if real_close(@b, line.b)
        self
      else
        NoPoints.new
      end
    else
      x = (line.b-@b)/(@m-line.m)
      y = @m * x + @b
      Point.new(x,y)
    end
  end

  def intersectVerticalLine vLine
    x = vLine.x
    y = @m * x + @b
    Point.new(x,y)
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end

  # inherits preprocess_prog and eval_prog
  # (which doesn't do anything, and just returns the object instantiation

end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x

  def initialize x
    @x = x
  end

  def shift(dx,dy)
    VerticalLine.new(@x+dx)
  end

  def intersect other # including NoPoints
    other.intersectVerticalLine self
  end

  def intersectPoint p
    p.intersectVerticalLine(self) # we can do this to re-use the Point case
  end

  def intersectLine line
    Line.intersectVerticalLine(self) # we can do this to re-use the Line case
  end

  def intersectVerticalLine vLine
    if real_close(@x, vLine.x)
      self
    else
      NoPoints.new
    end
  end

  def intersectWithSegmentAsLineResult seg
    seg
  end

  # inherits preprocess_prog and eval_prog 
  # (which doesn't do anything, and just returns the object instantiation)

end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and 
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2

  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end

  def shift(dx,dy)
    LineSegment.new(@x1+dx, @y1+dy, @x2+dx, @y2+dy)
  end

  def preprocess_prog
    if real_close(@x1,@x2) && real_close(@y1,@y2)
      Point.new(@x1,@y1)
    elsif (real_close(@x1,@x2) && @y1 > @y2) or (@x1 > @x2)
      LineSegment.new(@x2,@y2,@x1,@y1)
    else
      self
    end
  end

  # note for following methods below: "intersectLineSegment" is included as a
  # public method in GeometryValue class

  def intersect other
    other.intersectLineSegment self
  end

  def intersectPoint p
    p.intersectLineSegment self
  end

  def intersectLine line 
    line.intersectLineSegment self
  end

  def intersectVerticalLine vLine
    vLine.intersectLineSegment self
  end

  # note: hardest case (intersecting line segment with another line segment)
  def intersectWithSegmentAsLineResult seg
    seg1 = [@x1, @y1, @x2, @y2]
    seg2 = [seg.x1, seg.y1, seg.x2, seg.y2]
    if real_close(@x1, @x2) # the segments are on a vertical line
      s1x1, s1y1, s1x2, s1y2,
      s2x1, s2y1, s2x2, s2y2 = @y1 < seg.y1 ? seg1 + seg2 : seg2 + seg1
      if real_close(s1y2, s2y1)
        Point.new(s1x2, s1y2)
      elsif s1y2 < s2y1
        NoPoints.new
      elsif s1y2 > s2y2
        LineSegment.new(s2x1, s2y1, s2x2, s2y2)
      else
        LineSegment.new(s2x1, s2y1, s1x2, s1y2)
      end
    else # the segments are on a non-vertical line
      s1x1, s1y1, s1x2, s1y2,
      s2x1, s2y1, s2x2, s2y2 = @x1 < seg.x1 ? seg1 + seg2 : seg2 + seg1
      if real_close(s1x2, s2x1)
        Point.new(s1x2, s1y2)
      elsif s1x2 < s2x1
        NoPoints.new
      elsif s1x2 > s2x2
        LineSegment.new(s2x1, s2y1, s2x2, s2y2)
      else
        LineSegment.new(s2x1, s2y1, s1x2, s1y2)
      end
    end
  end

  # inherits eval_prog (which just returns the object instantiation)

end

# Note: there is no need for getter methods for the non-value classes

class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end

  # create a new Intersect object with the preprocessed expressions of e1, e2
  def preprocess_prog
    Intersect.new(@e1.preprocess_prog, @e2.preprocess_prog)
  end

  # evaluate e1 in the environment, and then see if it intersects with the
  # evaluation of e2 in the environment
  def eval_prog env
    @e1.eval_prog(env).intersect(@e2.eval_prog(env))
  end

end

# structure: Let s = e1 in e2
class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end

  # create a new Let expression with the preprocessed expressions of e1, e2
  def preprocess_prog
    Let.new(@s, @e1.preprocess_prog, @e2.preprocess_prog)
  end

  # evaluate e2 (which contains e1) after assigning s to e1 in a nested environment
  # and then update the outer environment by adding to it the nested
  def eval_prog env
    @e2.eval_prog([[s, @e1.eval_prog(env)]] + env)
  end

end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end

  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    pr[1]
  end

  def preprocess_prog
    self # no pre-processing to do here
  end

end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end

  # evaluate the expression "e" in the environment "env", and then shift "e" 
  def eval_prog env
    @e.eval_prog(env).shift(@dx,@dy)
  end

  # create a new Shift object with the preprocessed expression "e"
  def preprocess_prog
    Shift.new(@dx, @dy, @e.preprocess_prog)
  end

  # allows for nested shifts
  def shift(dx,dy)
    @e.shift(dx,dy)
  end

end
