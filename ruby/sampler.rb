require 'forwardable'

# An array-like class that automatically shuffles its original input so that it can be sampled in a
# pseudorandom order.
class SamplerArray
  include Enumerable
  extend Forwardable

  def_delegators :@ary, :[], :[]=, :push, :pop, :each

  # Constructs a SamplerArray by copying the given array.
  def initialize(ary)
    @ary = ary.shuffle
  end

  # Returns the underlying array structure. This does not copy the underlying structure, so modification
  # to the returned array will modify the SamplerArray structure.
  def to_a
    @ary
  end

  # Returns the underlying array structure. This does not copy the underlying structure, so modification
  # to the returned array will modify the SamplerArray structure.
  def to_ary
    to_a
  end

  # Samples an element from the SamplerArray, or +nil+ if there are no other elements. This will never return
  # the same element of the original list twice, unless it was duplicated in the original list.
  def sample
    pop
  end

end

# An infinite enumerator containing an array which, when iterated over, returns random elements, with
# replacement.
class SamplerEnumerator
  include Enumerable

  # Constructs an enumerator by copying the given array.
  def initialize(ary)
    @ary = ary.dup
  end

  # Samples a single element from the array.
  def sample
    @ary.sample
  end

  # Returns a lazy enumerator which infinitely samples the array.
  def each
    Enumerator::Lazy.new([1]) do |y|
      loop { y << sample }
    end
  end

end
