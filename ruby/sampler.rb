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
