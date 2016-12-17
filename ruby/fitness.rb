
# A fitness parameter is a tag which indicates certain details about a Location, which can be useful
# when determining where to place objects and other entities. Fitness parameters are usually created
# as combinations (using #+) of the pre-defined parameters below.
class Fitness
  attr_reader :treasure, :monster

  def initialize(tr, mo)
    @treasure = tr
    @monster = mo
  end

  # Combines two fitness parameters together.
  def +(fit)
    CompositeFitness.new self, fit
  end

  # The #+ operation forms a commutative (?) monoid with this object as the identity.
  def self.null
    @@nullFitness ||= Fitness.new 0.0, 0.0
  end

  def to_sxp
    [:fitness, @treasure, @monster].to_sxp
  end

  def self.from_sxp(arg)
    tr, mo = Reloader.assert_first :fitness, arg
    Fitness.new tr, mo
  end

end

# A combined fitness parameter, which merges together the effects of two other fitness parameters.
class CompositeFitness

  def initialize(a, b)
    @first = a
    @second = b
  end

  def treasure
    @first.treasure + @second.treasure
  end

  def monster
    @first.monster + @second.monster
  end

end

# This module consists of several useful predefined fitness parameters. While there is no issue with creating
# custom parameters, these are often sufficient for most cases.
module PredefFitness
  Uncivilized = Fitness.new 1.0, 1.0
  Outdoors = Fitness.new 1.0, 1.5
  HardToReach = Fitness.new 2.5, 2.5

  def predefFitnessFlags
    [:Uncivilized, :Outdoors, :HardToReach]
  end

  def self.included(base)
    predefFitnessFlags.each do |flag|
      unless base.const_defined? flag
        base.const_set flag, self.const_get(flag)
      end
    end
  end

end
