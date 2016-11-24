
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
    meta = MetaData.new(:':raw-nutrition' => @raw_nutrition,
                        :':raw-poison' => @raw_poison)
    [:food, name, :':full-name', full_name, :':plant-type', plant_type,
     :':nutritional-value', base_nutritional_value, :':poison-chance', base_poison_chance,
     :':meta', meta].to_sxp
  end

  def self.from_sxp(arg)
    name, *arr = Reloader.assert_first :food, arg
    ReloadedFood.new(name).tap do |rfd|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':full-name'
          rfd.full_name = v
        when :':plant-type'
          rfd.plant_type = v
        when :':nutritional-value', :':poison-chance'
          # Ignore
        when :':meta'
          meta = Reloader.load v
          rfd.raw_nutrition = meta[:':raw-nutrition']
          rfd.raw_poison = meta[:':raw-poison']
        end
      end
    end
  end

end

class ReloadedFood < Food
  attr_accessor :name, :full_name, :plant_type, :raw_nutrition, :raw_poison

  def initialize(name)
    self.name = name
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

  def self.from_sxp(arg)
    name, *arr = Reloader.assert_first :plant, arg
    type = nil
    food = nil
    time = nil
    Reloader.hash_like(arr) do |k, v|
      case k
      when :':type'
        type = v
      when :':food'
        food = Reloader.load v
      when :':growth-time'
        time = v
      end
    end
    ReloadedPlant.new(type, food, time).tap do |pl|
      pl.name = name
    end
  end

end

class ReloadedPlant < Plant

  def initialize(type, food, growth_time = nil)
    super
  end

  def name=(val)
    @name = val.to_s
  end

end
