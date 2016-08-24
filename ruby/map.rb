
require 'forwardable'
require 'sxp'

class Map
  extend Forwardable
  include Enumerable

  def_delegators :@ary, :each, :push

  def initialize(ary = [])
    @ary = ary.to_a
  end

  def to_ary
    @ary
  end

  def to_a
    to_ary
  end

  def to_sxp
    ([:map] + to_ary).to_sxp
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first:map, arg
    Map.new Reloader.list_like(arr)
  end

  def [](val)
    to_ary.find { |x| x.id == val }
  end

  def put_somewhere(obj, type = Object, &block)
    block = proc { true } unless block
    weight = Proc.new { |x| 1 / (x.count_items(type) + 1) }
    total = select( &block ).map( &weight ).reduce(:+)
    num = rand total
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

class Location
  extend Forwardable
  include Enumerable

  attr_reader :id, :name, :country_name, :generic_name

  def_delegators :@contents, :each, :push, :delete
  def_delegator :@links, :each, :each_link

  def initialize(id, name, country_name, generic_name: nil, valid_creatures: nil, valid_plants: nil)
    @id = id
    @name = name
    @country_name = country_name
    @generic_name = generic_name
    @contents = []
    @links = []
    @valid_creatures = valid_creatures
    @valid_plants = valid_plants
  end

  def can_have_creatures?
    not @valid_creatures.nil?
  end

  def can_have?(x)
    case x
    when @valid_creatures then true
    when @valid_plants then true
    else false
    end
  end

  def long_name
    if country_name
      "#{name}, #{country_name}"
    else
      name
    end
  end

  def add_link(x)
    @links.push x unless @links.include? x
  end

  def remove_link(x)
    @links.delete x
  end

  def civilized?
    not can_have_creatures?
  end

  def to_sxp
    prefix = [:location, id, name]
    country = country_name ? [:':country', country_name] : []
    links = [:':links', each_link.to_a]
    contents = [:':contents', each.to_a]
    civilized = [:':civilized', civilized?]
    meta = [:':meta', MetaData.new(:':generic-name' => generic_name,
                                   :':creatures' => valid_creatures_wrapper,
                                   :':plants' => valid_plants_wrapper)]
    (prefix + country + links + contents + civilized + meta).to_sxp
  end

  def self.from_sxp(arg)
    id, name, *arr = Reloader.assert_first :location, arg
    Location.new(id, name, nil).tap do |loc|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':country'
          loc.instance_variable_set :@country_name, v
        when :':links'
          loc.instance_variable_set :@links, v
        when :':contents'
          elems = v.collect { |x| Reloader.load x }
          loc.instance_variable_set :@contents, elems
        when :':civilized'
          # Ignore this arg; we'll get the necessary info from :meta
        when :':meta'
          meta = Reloader.load v
          loc.instance_variable_set :@generic_name, meta[:':generic-name']
          loc.instance_variable_set :@valid_creatures, Reloader.load(meta[:':creatures']).data
          loc.instance_variable_set :@valid_plants, Reloader.load(meta[:':plants']).data
        end
      end
    end
  end

  def count_items(type = Item)
    self.count { |x| x.kind_of? type }
  end

  def valid_creatures_wrapper
    ValidityWrapper.new @valid_creatures
  end

  def valid_plants_wrapper
    ValidityWrapper.new @valid_plants
  end

end
