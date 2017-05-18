
require 'forwardable'
require 'sxp'

# ///// Use the fitness parameters when generating the world

# The map contains a collection of Location instances, which are interconnected with one another.
class Map
  extend Forwardable
  include Enumerable
  include DeltaAble

  def_delegators :@ary, :each, :push

  # Initializes a map from the given collection of locations. The collection defaults to the empty
  # list. If provided, the argument should be a collection of Location objects which responds to #to_a.
  def initialize(ary = [])
    @ary = ary.to_a
  end

  def each_new_node(&block)
    @ary.each(&block)
  end

  def to_delta
    DeltaMap.new self
  end

  # Returns the underlying list structure. The individual elements of the returned list can be modified, but
  # the list itself should be left unmodified.
  def to_ary
    @ary
  end

  # Returns the underlying list structure. The individual elements of the returned list can be modified, but
  # the list itself should be left unmodified.
  def to_a
    to_ary
  end

  def to_sxp
    ([:map] + to_ary).to_sxp
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :map, arg
    Map.new Reloader.list_like(arr)
  end

  # Retrieves a location by ID value, or +nil+ if no matching location exists.
  def [](val)
    to_ary.find { |x| x.id == val }
  end

  # Places +obj+ somewhere on the map. The block and type arguments are both optional. This method
  # will weigh the locations when choosing a random position so that locations which have objects
  # already in them are less likely to be chosen. If +type+ is provided, it should be a class type
  # (or other object which responds to #===) which restricts the object which are considered when
  # counting the existing objects in the world. If the block is provided, it should take one
  # argument, a Location, and return whether or not that location should be considered at all.
  def put_somewhere(obj, type = Object, &block)
    block = proc { true } unless block
    weight = Proc.new { |x| 1 / (x.count_items(type) + 1) }
    total = select( &block ).map( &weight ).reduce(:+)
    total = 1.0 if total.nil?
    num = Random.rand total
    result = detect do |x|
      next unless block.call x
      num -= weight.(x)
      num <= 0
    end
    if result
      result.push obj
      result
    else
      nil
    end
  end

end

# A location on the map. Locations keep track of what is allowed to spawn in them, as well as what
# other locations they are connected to.
class Location
  extend Forwardable
  include Enumerable
  include DeltaAble

  attr_reader :id, :name, :country_name, :generic_name, :water_mode, :fitness

  def_delegators :@contents, :each, :push, :delete
  def_delegator :@links, :each, :each_link

  # Initializes a location, given an ID, a name, and a country name. If a generic name is provided,
  # it will be used when referring to the general location colloquially and is often simply the
  # country name. The +valid_creatures+ and +valid_plants+ arguments restrict which animals and plants
  # can spawn in this location, respectively. If not provided, no creatures or animals are allowed at
  # the position.
  def initialize(id, name, country_name,
                 generic_name: nil,
                 valid_creatures: nil,
                 valid_plants: nil,
                 fitness: Fitness.null)
    @id = id
    @name = name
    @country_name = country_name
    @generic_name = generic_name
    @contents = []
    @links = []
    @valid_creatures = (valid_creatures || EmptyValidator.new)
    @valid_plants = (valid_plants || EmptyValidator.new)
    @water_mode = nil
    @fitness = fitness
  end

  def to_delta
    DeltaLocation.new self
  end

  # Returns whether or not the location is allowed to have any creatures.
  def can_have_creatures?
    not @valid_creatures.empty?
  end

  # Given a plant or animal, returns whether or not the location is allowed to house that plant or
  # animal.
  def can_have?(x)
    case
    when (@valid_creatures.include? x) then true
    when (@valid_plants.include? x) then true
    else false
    end
  end

  # Returns the full name of the location, including the short name and the country name.
  def long_name
    if country_name
      "#{name}, #{country_name}"
    else
      name
    end
  end

  # Adds a link from this location to another location. Note that this is a one-directional
  # link by default, meaning that if a symmetrical link is desired, #add_link should be called
  # on both ends of the link.
  def add_link(x)
    @links.push x unless @links.include? x
  end

  # Removes the link to the given location.
  def remove_link(x)
    @links.delete x
  end

  # Returns whether or not the location is considered civilized. A civilized location is
  # one which forbids the spawning of any creatures.
  def civilized?
    not can_have_creatures?
  end

  # Marks the location as non-water, which is the default. Swimming creatures cannot move onto
  # dry land.
  def mark_as_dry
    @water_mode = nil
  end

  # Marks the location as being by the seaside. A shore location will allow water and
  # land creatures onto it.
  def mark_as_shore
    @water_mode = :shore
  end

  # Marks the location as being a sea. A sea location will not allow land creatures,
  # including the player, to traverse it.
  def mark_as_sea
    @water_mode = :sea
  end

  # Marks the water mode to one of +nil+, +:shore+, or +:sea+. If an invalid argument is
  # provided, the water mode is not set.
  def mark_water(water_mode)
    case water_mode
    when nil then mark_as_dry
    when :shore then mark_as_shore
    when :sea then mark_as_sea
    end
  end

  def to_sxp
    prefix = [:location, id, name]
    country = country_name ? [:':country', country_name] : []
    links = [:':links', each_link.to_a]
    contents = [:':contents', each.to_a]
    civilized = [:':civilized', civilized?]
    water = [:':water', water_mode]
    fitness_key = [:':fitness', fitness]
    meta = [:':meta', MetaData.new(:':generic-name' => generic_name,
                                   :':creatures' => valid_creatures,
                                   :':plants' => valid_plants)]
    (prefix + country + links + contents + civilized + water + fitness_key + meta).to_sxp
  end

  def self.from_sxp(arg)
    id, name, *arr = Reloader.assert_first :location, arg
    ReloadedLocation.new(id, name, nil).tap do |loc|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':country'
          loc.country_name = v
        when :':links'
          v.each { |lnk| loc.add_link lnk }
        when :':contents'
          elems = v.collect { |x| Reloader.load x }
          elems.each { |obj| loc.push obj }
        when :':civilized'
          # Ignore this arg; we'll get the necessary info from :meta
        when :':water'
          loc.mark_water v
        when :':fitness'
          loc.fitness = Reloader.load v
        when :':meta'
          meta = Reloader.load v
          loc.generic_name = meta[:':generic-name']
          loc.valid_creatures = Reloader.load(meta[:':creatures'])
          loc.valid_plants = Reloader.load(meta[:':plants'])
        end
      end
    end
  end

  # Counts the number of items in the location matching +type+, which should respond to #===.
  def count_items(type = Item)
    self.count { |x| x.kind_of? type }
  end

  # Returns an unspecified object which can be serialized using #to_sxp and stores the information
  # regarding which creatures are valid in the location.
  def valid_creatures
    @valid_creatures
  end

  # Returns an unspecified object which can be serialized using #to_sxp and stores the information
  # regarding which plants are valid in the location.
  def valid_plants
    @valid_plants
  end

end

class ReloadedLocation < Location
  attr_writer :country_name, :generic_name, :valid_creatures, :valid_plants, :fitness

  def initialize(id, name, country_name, generic_name: nil, valid_creatures: nil, valid_plants: nil)
    super
  end

end
