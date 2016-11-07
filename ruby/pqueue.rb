
# ///// Test this

# A (minimum) priority queue class, for storing things and then popping them in some priority order.
class PriorityQueue

  # Constructs an empty priority queue, given an ordering procedure as a block. The ordering procedure
  # should take two arguments and return a Boolean, and it should satisfy the standard properties
  # associated with a (irreflexive) total ordering.
  def initialize(&block)
    @arr = []
    @ordering = (block || :<.to_proc)
  end

  # Constructs a priority queue, according to #initialize, and then places elements into it immediately.
  def self.[](*elems, &block)
    PriorityQueue.new(&block).tap do |pq|
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

end
