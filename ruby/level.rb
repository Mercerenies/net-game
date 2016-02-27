
class Level

  def self.individual
    @@individual ||= LevelZero.new
  end

  def self.city
    @@city ||= LevelOne.new 1..6
  end

  def self.district
    @@district ||= LevelOne.new 4..8
  end

  def self.state
    @@state ||= LevelTwo.new 2..4
  end

  def self.country
    @@country ||= LevelTwo.new 4..8
  end

  def self.[](*args)
    self.new(*args)
  end

  def initialize(num)
    @number = num
  end

  def to_i
    @number
  end

  def to_int
    @number
  end

  def make_children
    []
  end

  def level_up
    nil
  end

end

class LevelZero < Level

  def initialize
    super 0
  end

  def level_up
    Level.city
  end

  def to_s
    "LevelZero[]"
  end

end

class LevelOne < Level
  attr_accessor :node_count

  def initialize(node_count)
    super 1
    @node_count = node_count
  end

  def make_children
    count = case @node_count
            when Integer then @node_count
            when Range then @node_count.to_a.sample
            else @node_count.to_i
            end
    count.times.collect { Level.individual }
  end

  def level_up
    Level.country
  end

  def to_s
    "LevelOne[#{@node_count}]"
  end

end

class LevelTwo < Level
  attr_accessor :node_count

  def initialize(node_count)
    super 2
    @node_count = node_count
  end

  def make_children
    count = case @node_count
            when Integer then @node_count
            when Range then @node_count.to_a.sample
            else @node_count.to_i
            end
    count.times.collect { [Level.city, Level.district].sample }
  end

  def level_up
    nil
  end

  def to_s
    "LevelTwo[#{@node_count}]"
  end

end

