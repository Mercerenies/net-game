
class Food
  attr_reader :name, :full_name, :plant_type

  def initialize(data)
    @name = Util.titlecase data.name
    @name = @name.gsub(/ (?:tree|plant|bush|flower)$/i, '');
    @full_name = data.full_name
    @plant_type = data.plant
    @raw_nutrition = data.nutrition
    @raw_poison = data.poison
  end

  def base_nutritional_value
    # Equation derived using f(x) = A ln(x + B), f(0) = 1, f(5) = 2
    0.97418106345 * Math.log(@raw_nutrition + 2.79129)
  end

  def base_poison_chance
    # Equation derived using f(x) = 1 + A e^(- B x), f(0) = 0.01, f(5) = 0.50
    1 - 0.99 * Math.exp(- 0.136619 * @raw_poison)
  end

  def to_sxp
    [:food, name, :':full-name', full_name, :':plant-type', plant_type,
     :':nutritional-value', base_nutritional_value, :':poison-chance', base_poison_chance].to_sxp
  end

end

class Plant
  attr_reader :name, :food, :type, :growth_time

  def self.suffix(type)
    " #{type.to_s.capitalize}"
  end

  def initialize(type, food, growth_time = nil)
    @name = Util.titlecase(food.name) + Plant.suffix(type)
    @type = type
    @food = food
    @growth_time = growth_time || rand(3..7)
  end

  def to_sxp
    [:plant, name, :':type', type, :':food', food, :':growth-time', growth_time].to_sxp
  end

end

class Plants

  def initialize(ary)
    @ary = ary
  end

  def self.[](*args)
    Plants.new args
  end

  def to_ary
    @ary
  end

  def to_a
    to_ary
  end

  def ===(obj)
    obj.respond_to? :plant_type and @ary.include? obj.plant_type
  end

end
