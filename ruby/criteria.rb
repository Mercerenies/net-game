
class CriteriaQueue
  include Enumerable

  def initialize(ary = [])
    @ary = ary.to_a
  end

  def self.[](*args)
    CriteriaQueue.new args
  end

  def each(&block)
    @ary.each(&block)
  end

  def to_ary
    @ary
  end

  def to_a
    to_ary
  end

  def push(x)
    @ary.push x
  end

  def <<(x)
    push x
  end

  # This is what makes this particular queue different; #shift can take a block
  # and will remove the first element of the queue which satisfies the block
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
