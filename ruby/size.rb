
# TODO Document this class and its methods
# TODO At present, the population boundaries are fixed based on sample data; make them adaptive

# Population contains convenience methods for operating on population sizes.
class Population
  include Comparable

  attr_reader :rank, :begin, :end, :label

  # The constructor seldom need be called directly. There are convenience methods provided
  # to obtain and compare existing population instances.
  def initialize(r, t1, t2, l)
    @rank = r
    @begin = t1
    @end = t2
    @label = l
  end

  def include?(size)
    return false unless size
    (self.begin.nil? or self.begin <= size) and
      (self.end.nil? or size <= self.end)
  end

  def <=>(other)
    self.rank <=> other.rank
  end

  def to_s
    "#<Population #{rank} #{label}>"
  end

  # Given a numerical population, determines the approximate size of the area.
  # The value returned will be a Population instance.
  def self.size(size)
    sizes.detect { |x| x.include? size }
  end

  def self.sizes
    [small_city, moderate_city, large_city, small_country, large_country]
  end

  def self.small_city
    @@small_city ||= Population.new(0, 1, 480323.25, :small_city)
  end

  def self.moderate_city
    @@moderate_city ||= Population.new(1, 480323.25, 3760336.25, :moderate_city)
  end

  def self.large_city
    @@large_city ||= Population.new(2, 3760336.25, 12000000, :large_city)
  end

  def self.small_country
    @@small_country ||= Population.new(3, 12000000, 44500000, :small_country)
  end

  def self.large_country
    @@large_country ||= Population.new(4, 44500000, nil, :large_country)
  end

end
