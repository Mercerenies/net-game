
# A fitness parameter is a tag which indicates certain details about a Location, which can be useful
# when determining where to place objects and other entities. Fitness parameters are usually created
# as combinations (using #+) of the pre-defined parameters below.
class Fitness
  attr_reader :treasure, :monster

  def initialize(treasure:, monster:)
    @treasure = treasure
    @monster = monster
  end

  # Combines two fitness parameters together.
  def +(fit)
    CompositeFitness.new self, fit
  end

  # The #+ operation forms a commutative (?) monoid with this object as the identity.
  def self.null
    @@nullFitness ||= Fitness.new treasure: 0.0, monster: 0.0
  end

  def to_sxp
    [:fitness, treasure, monster].to_sxp
  end

  def self.from_sxp(arg)
    tr, mo = Reloader.assert_first :fitness, arg
    Fitness.new treasure: tr, monster: mo
  end

end

# A combined fitness parameter, which merges together the effects of two other fitness parameters.
class CompositeFitness < Fitness

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

# This module consists of several useful predefined fitness parameters. While there is no issue
# with creating custom parameters, these are often sufficient for most cases.
module PredefFitness
  Uncivilized = Fitness.new treasure: 1.0, monster: 1.0
  Outdoors = Fitness.new treasure: 1.0, monster: 1.5
  HardToReach = Fitness.new treasure: 2.5, monster: 2.5
  Safekeeping = Fitness.new treasure: 1.5, monster: 0.0
  VeryHardToReach = Fitness.new treasure: 2.5, monster: 2.5

  def self.predefFitnessFlags
    [:Uncivilized, :Outdoors, :HardToReach, :Safekeeping, :VeryHardToReach]
  end

  def self.included(base)
    predefFitnessFlags.each do |flag|
      unless base.const_defined? flag
        base.const_set flag, self.const_get(flag)
      end
    end
  end

end
