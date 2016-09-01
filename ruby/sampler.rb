require 'forwardable'

class SamplerArray
  include Enumerable
  extend Forwardable

  def_delegators :@ary, :[], :[]=, :push, :pop, :each

  def initialize(ary)
    @ary = ary.shuffle
  end

  def to_a
    @ary
  end

  def to_ary
    to_a
  end

  def sample
    pop
  end

end

class SamplerEnumerator
  include Enumerable

  def initialize(ary)
    @ary = ary
  end

  def sample
    @ary.sample
  end

  def each
    Enumerator::Lazy.new([1]) do |y|
      loop { y << sample }
    end
  end

end
