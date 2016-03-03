
class Player

  def to_sxp
    [:'player'].to_sxp
  end

end

class WarpPoint

  def to_sxp
    [:'warp-point'].to_sxp
  end

end

class Item
end

class Weapon < Item
  attr_reader :name, :type

  def initialize(name, type)
    @name = name
    @type = type
  end

  def to_sxp
    [:weapon, @name, :':type', @type].to_sxp
  end

end
