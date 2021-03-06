
class Person
end

class Player < Person

  def to_sxp
    [:'player'].to_sxp
  end

  def self.from_sxp(arg)
    Reloader.assert_first :'player', arg
    Player.new
  end

end

class WarpPoint

  def to_sxp
    [:'warp-point'].to_sxp
  end

  def self.from_sxp(arg)
    Reloader.assert_first :'warp-point', arg
    WarpPoint.new
  end

end

class Item
  attr_reader :name, :flags, :weight

  def initialize(name, i_weight = 1)
    @name = name
    @flags = []
    self.weight = i_weight
  end

  def add_flags(*args)
    @flags.push(*args)
  end

  def weight=(val)
    @weight = val.to_i
    @weight = 1 if @weight < 1
  end

  def to_sxp
    [:item, name, :':weight', weight, :':flags', flags].to_sxp
  end

  def self.from_sxp(arg)
    name, *arr = Reloader.assert_first :item, arg
    Item.new(name).tap do |item|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':weight'
          item.weight = v
        when :':flags'
          item.add_flags *v
        end
      end
    end
  end

  def self.make_random(&block)
    name = Util.titlecase RandomItemList.sample
    name = block.(name) if block
    Item.new name
  end

end

module RandomItemList

  def self.sample
    RandomItemList.data_set.sample
  end

  def self.load_data_set(filename = "./data/random.txt")
    @@data = File.open filename do |f|
      f.each.collect(&:chomp).to_a
    end
  end

  def self.data_set
    @@data ||= RandomItemList.load_data_set
  end

end
