
class Person
end

class Player < Person

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
  attr_reader :name, :flags

  def initialize(name)
    @name = name
    @flags = []
  end

  def add_flags(*args)
    @flags.push(*args)
  end

  def to_sxp
    [:item, name, :':flags', flags].to_sxp
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
