
# TODO If this becomes inefficient, we could use a heap implementation instead of an array.

# A (minimum) priority queue class, for storing things and then popping them in some priority order.
class PriorityQueue
  include Enumerable

  # Constructs an empty priority queue, given an ordering procedure as a block. The ordering procedure
  # should take two arguments and return a Boolean, and it should satisfy the standard properties
  # associated with a (irreflexive) total ordering.
  def initialize(&block)
    @arr = []
    @ordering = (block || :<.to_proc)
  end

  # Constructs a priority queue with the default ordering and then places elements into it immediately.
  def self.[](*elems)
    PriorityQueue.new.tap do |pq|
      elems.each { |e| pq.add e }
    end
  end

  # Adds one or more elements to the priority queue, in priority order
  def add(*elems)
    elems.each do |e|
      i = @arr.find_index { |o| @ordering.call e, o }
      i ||= @arr.size
      @arr.insert i, e
    end
    self
  end

  # An infix operator form that calls #add.
  def <<(elem)
    self.add elem
  end

  # Enumerate each element of the underlying array in order
  def each(&block)
    @arr.each &block
  end

  # Pops the item with the lowest priority out of the list and returns it, returning +nil+
  # if the list is empty. Note that both #pop and #peek preserve the FILO ordering of elements
  # with the same priority.
  def pop
    @arr.shift
  end

  # Returns the lowest priority item without popping it. Returns +nil+ if the list is empty.
  # Note that both #pop and #peek preserve the FILO ordering of elements with the same priority.
  def peek
    @arr[0]
  end

  # Returns whether or not the priority queue contains elements.
  def empty?
    @arr.empty?
  end

end
