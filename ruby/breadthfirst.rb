
# A module for building BFS algorithm instances.
module BFSBuilder

  class Algorithm

    def initialize(cost:, check:, branch:)
      @cost = cost
      @check = check
      @branch = branch
    end

    # Assigns the cost function. The cost function should be a 1-ary block which takes
    # an element of the search space and returns a real number indicating the cost of
    # that element.
    def cost(&block)
      Algorithm.new(cost: block, check: @check, branch: @branch)
    end

    # Assigns the check function. The check function should be a 1-ary block which takes
    # an element of the search space and returns a boolean.
    def check(&block)
      Algorithm.new(cost: @cost, check: block, branch: @branch)
    end

    # Assigns the branch function. The branch function should be a 1-ary block which takes
    # an element of the search space and returns a list of new elements to add to the frontier.
    def branch(&block)
      Algorithm.new(cost: @cost, check: @check, branch: block)
    end

    def cost_of(x)
      @cost.(x)
    end

    def valid?(x)
      @check.(x)
    end

    def expand(x)
      @branch.(x)
    end

  end

end

# An instance of BFS::Algorithm that is provided for convenience and intended to be used as the
# entry-point to the builder.
BFSAlgorithm = BFSBuilder::Algorithm.new(cost: nil, check: nil, branch: nil)

# A breadth-first-search algorithm instance. The BFSBuilder::Algorithm object given to the initializer
# determines the specific attributes of the algorithm.
class BFS

  # Constructs a BFS instance with the given initial node, which can be any object in the search space,
  # and the algorithm configuration, as an instance of BFSBuilder::Algorithm.
  def initialize(node, algorithm)
    @algorithm = algorithm
    @frontier = PriorityQueue.new { |x, y| @algorithm.cost_of(x) < @algorithm.cost_of(y) }
    @frontier << node
    @terminated = false
    @result = nil
  end

  # Returns whether the search has completed.
  def terminated?
    @terminated
  end

  # Returns the result of the search, or +nil+ if the search is still running. Note that it is strongly
  # recommended (though not required) that +nil+ not be in the search space of the algorithm,
  # for #result will return +nil+ in the following three cases.
  # * If the search has yet to terminate.
  # * If the search has terminated and failed to find a solution.
  # * If the search has terminated and found +nil+ to be a solution.
  # The first case can be resolved by checking #terminate?, but the latter two cases are
  # indistinguishable.
  def result
    if terminated?
      @result
    else
      nil
    end
  end

  def result=(x)
    @terminated = true
    @result = x
  end

  # Runs one step of the algorithm.
  def run_step
    unless terminated?
      if @frontier.empty?
        self.result = nil
      else
        current = @frontier.pop
        if @algorithm.valid? current
          self.result = current
        else
          @algorithm.expand(current).each { |elem| @frontier.add elem }
        end
      end
    end
  end

  # Runs +n+ steps of the algorithm.
  def run_times(n)
    n.times do
      break if terminated?
      self.run_step
    end
  end

  # Runs infinitely many steps of the algorithm, stopping only when the search space is exhausted or a
  # result has been found. Care should be taken that this method is not called on search spaces which
  # are infinite and may lack a solution, for this method will infinitely loop in that particular case.
  def run_until_done
    loop do
      break if terminated?
      self.run_step
    end
  end

  private :result=

end
