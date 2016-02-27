
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

  def self.instance
    @@instance ||= AffixSet.new
  end

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

  def sampler(name, number)
    SamplerArray.new apply(name, number)
  end

end
