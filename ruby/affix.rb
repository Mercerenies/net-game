
# An Affix contains a prefix/suffix pair which can be applied to names of places or other objects.
class Affix
  attr_reader :lhs, :rhs

  # Initializes a prefix/suffix pair.
  def initialize(lhs, rhs)
    @lhs = lhs
    @rhs = rhs
  end

  # Decorates an arbitrary string with the affix pair.
  def decorate(name)
    @lhs + name + @rhs
  end

end

# An object managing a set of affixes. A default instance is provided for city affixes.
class AffixSet

  # Returns the default instance for city affixes, constructing it on-demand if it does not exist yet.
  def self.instance
    @@instance ||= AffixSet.new
  end

  # Constructs an affix set from a file of the appropriate format. Each line of the given file
  # should be of the form
  #  (("prefix1" . "suffix1") ("prefix2" . "suffix2") ...)
  # The affixes of a single line will form a single possible collection in the AffixSet.
  def initialize(filename = "./data/naming_inner.txt")
    @data = File.open filename do |f|
      f.each.collect do |line|
        line.scan(/\("([^"]*)" \. "([^"]*)"\)/).collect { |lhs, rhs| Affix.new lhs, rhs }
      end
    end
  end

  # Returns a simple printed representation, for debugging purposes.
  def inspect
    to_s
  end

  # The value returned from #to_a should not be modified.
  def to_a
    @data
  end

  # The value returned from #to_ary should not be modified.
  def to_ary
    to_a
  end

  # Collects the affix set into a hash, indexed by the size. The value returned by #to_h should not
  # be modified.
  def to_h
    hash = {}
    @data.each do |xs|
      hash[xs.size] ||= []
      hash[xs.size] << xs
    end
    hash
  end

  # Given a set of names, selects a collection of affixes from the affix set of matching size and
  # decorates each name with a unique affix from that collection. Signals an error if there is no
  # affix collection of a corresponding size.
  def collate(names)
    self.to_h[names.size].sample.zip(names).collect { |a, n| a.decorate n }
  end

  # Applies +number+ different affixes to +name+, returning a list of results. Signals an error if there
  # is no corresponding affix collection of an appropriate size.
  def apply(name, number)
    collate(number.times.collect { name })
  end

  # Applies +number+ different affixes to +name+, in the same way as #apply, but returning a SamplerArray
  # instance, not a normal array.
  def sampler(name, number)
    SamplerArray.new apply(name, number)
  end

end
