
# An implementation of a FIFO queue in which the action of dequeuing can include a predicate block, in which
# case the front element satisfying the predicate will be popped and returned, not simply the front element
# overall.
class CriteriaQueue
  include Enumerable

  # Initializes a criteria queue from the given array-like object. The argument, if supplied, should have
  # +to_a+. The elements nearer the front of the array-like object are considered to be at the front of the
  # queue.
  def initialize(ary = [])
    @ary = ary.to_a
  end

  # Constructs a criteria queue containing the given elements. The front of the queue will consist of
  # the elements nearer the beginning of the argument list.
  def self.[](*args)
    CriteriaQueue.new args
  end

  # Iterates over the criteria queue, from front to back.
  def each(&block)
    @ary.each(&block)
  end

  # Returns the underlying array, not copied.
  def to_ary
    @ary
  end

  # Returns the underlying array, not copied.
  def to_a
    to_ary
  end

  # Pushes a single element onto the back of the queue.
  def push(x)
    @ary.push x
  end

  # Pushes a single element onto the back of the queue.
  def <<(x)
    push x
  end

  # With no block, #shift pops and returns the front element of the queue. With a block argument,
  # #shift seeks the frontmost element of the queue for which the block returns truthy, popping
  # that element and returning it. In either case, if no matching element exists, this method returns
  # +nil+.
  def shift(&block)
    return @ary.shift unless block
    index = @ary.find_index(&block)
    if index
      @ary.delete_at index
    else
      nil
    end
  end

end
