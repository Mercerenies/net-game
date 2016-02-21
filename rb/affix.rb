
class Affix
  attr_reader :lhs, :rhs

  def initialize(lhs, rhs)
    @lhs = lhs
    @rhs = rhs
  end

  def decorate(name)
    @lhs + name + @rhs
  end

end

class AffixSet

  def initialize(filename = "./data/naming_inner.txt")
    @data = File.open filename do |f|
      f.each.collect do |line|
        line.scan(/\("([^"]*)" \. "([^"]*)"\)/).collect { |lhs, rhs| Affix.new lhs, rhs }
      end
    end
  end

  def inspect
    to_s
  end

  def to_a
    @data
  end

  def to_ary
    to_a
  end

  def to_h
    hash = {}
    @data.each do |xs|
      hash[xs.size] ||= []
      hash[xs.size] << xs
    end
    hash
  end

  def collate(names)
    self.to_h[names.size].sample.zip(names).collect { |a, n| a.decorate n }
  end

  def apply(name, number)
    collate(number.times.collect { |i| name })
  end

  # ///// Converting the generator to Ruby and having the runner stay in Lisp
  #       Remember that we're in a branch in Git right now, so don't treat it
  #       like master.

end
