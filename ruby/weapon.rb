
module WeaponMod

  @@words = [:unwieldy, :wieldy, :weak, :strong]

  def self.sample
    @@words.sample
  end

  def self.choose
    if rand < 0.85
      self.sample
    else
      []
    end
  end

end

class Weapon < Item
  attr_reader :name, :type

  def initialize(name, type)
    @name = name
    @type = type
    @modifier = WeaponMod.choose
  end

  def to_sxp
    [:weapon, @name, :':type', @type, :':mod', @modifier].to_sxp
  end

end
