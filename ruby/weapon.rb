
module WeaponMod

  @@words = [:unwieldy, :wieldy, :weak, :strong]

  def self.sample
    @@words.sample
  end

  def self.choose
    if rand < 0.85
      self.sample
    else
      nil
    end
  end

end

class Weapon < Item
  attr_reader :type

  def initialize(name, type)
    super(name)
    @type = type
    @modifier = WeaponMod.choose
  end

  def to_sxp
    [:weapon, name, :':type', @type, :':mod', @modifier, :':flags', flags].to_sxp
  end

  def self.from_sxp(arg)
    name, *arr = Reloader.assert_first :weapon, arg
    ReloadedWeapon.new(name).tap do |wpn|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':type'
          wpn.type = v
        when :':mod'
          wpn.modifier = v
        when :':flags'
          wpn.add_flags *v
        end
      end
    end
  end

end

class ReloadedWeapon < Weapon
  attr_writer :type, :modifier

  def initialize(name)
    super name, nil
  end

end
