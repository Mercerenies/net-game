require 'forwardable'

class CreatureSet
  include Enumerable

  def initialize
    @animals = []
    @monsters = []
  end

  # Returns whether it was able to successfully load data from the page
  def load_from_page(x)
    case x
    when AnimalPage
      push Animal.new(x)
    when MonsterPage
      push Monster.new(x)
    end
  end

  def self.standard_spawn_cycle?(x)
    case x
    when AnimalPage
      true
    else
      false
    end
  end

  def push(x)
    case x
    when Animal
      @animals << x
      return x
    when Monster
      @monsters << x
      return x
    end
    nil
  end

  def each(&block)
    if block.nil?
      Enumerator.new do |y|
        each_animal { |e| y << e }
        each_monster { |e| y << e }
      end
    else
      each_animal &block
      each_monster &block
    end
  end

  def each_animal(&block)
    @animals.each &block
  end

  def each_monster(&block)
    @monsters.each &block
  end

  def to_sxp
    ([:'creature-set'] + to_a).to_sxp
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :'creature-set', arg
    CreatureSet.new.tap do |set|
      Reloader.list_like(arr) { |x| set.push x }
    end
  end

  def empty?
    @animals.empty? and @monsters.empty?
  end

end

class Creature
  attr_reader :id

  def initialize
    @id = Node.get_id
  end

  def to_sxp
    [:creature].to_sxp
  end

end

class Animal < Creature
  extend Forwardable

  attr_reader :meat_type

  def_delegators :@page, :pack, :speed, :threat, :size

  def initialize(data)
    super()
    @page = data
    @meat_type = Food.generate( # TODO Set nutrition and poison amounts intelligently
      name: "#{name} Meat",
      full_name: "#{name} Meat",
      source_type: :animal,
      raw_nutrition: 1.0,
      raw_poison: 0.0
    )
  end

  def name
    Util.titlecase @page.name
  end

  def air?
    @page.air
  end

  def sea?
    @page.sea
  end

  def to_sxp
    [:animal, id, name, :':pack', pack, :':speed', speed, :':threat', threat,
     :':size', size, :':air', air?, :':sea', sea?, :':meat-type', meat_type].to_sxp
  end

  def self.from_sxp(arg)
    id, name, *arr = Reloader.assert_first :animal, arg
    ReloadedAnimal.new.tap do |anim|
      anim.id = id
      anim.name = name
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':pack'
          anim.pack = v
        when :':speed'
          anim.speed = v
        when :':threat'
          anim.threat = v
        when :':size'
          anim.size = v
        when :':air'
          anim.air = v
        when :':sea'
          anim.sea = v
        when :':meat-type'
          anim.meat_type = v
        end
      end
    end
  end

end

class ReloadedAnimal < Animal
  extend Forwardable

  attr_accessor :id, :name, :pack, :speed, :threat, :size, :air, :sea, :meat_type

  def initialize
    super(nil)
    @id = nil
    @name = ''
    @pack = 0
    @speed = 0
    @threat = 0
    @size = 0
    @air = false
    @sea = false
  end

  def air?
    @air
  end

  def sea?
    @sea
  end

end

class Monster < Creature
  extend Forwardable

  attr_reader :type, :type_name

  def_delegators :@page, :affinity, :chaos

  def initialize(data)
    super()
    @page = data
    @type_name, @type = data.info.to_a.sample if data
  end

  def name
    Util.titlecase @page.name
  end

  def to_sxp
    [:monster, id, name, :':affinity', affinity, :':chaos', chaos,
     :':type', type, :':type-name', type_name].to_sxp
  end

  def self.from_sxp(arg)
    id, name, *arr = Reloader.assert_first :monster, arg
    ReloadedMonster.new.tap do |mon|
      mon.id = id
      mon.name = name
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':affinity'
          mon.affinity = v
        when :':chaos'
          mon.chaos = v
        when :':type'
          mon.type = v
        when :':type-name'
          mon.type_name = v
        end
      end
    end
  end

end

class MonsterInstance
  attr_reader :id

  def initialize(id)
    @id = id
  end

  def to_sxp
    [:'monster-instance', id].to_sxp
  end

  def self.from_sxp(arg)
    id, *ignore = Reloader.assert_first :'monster-instance', arg
    MonsterInstance.new id
  end

end

class ReloadedMonster < Monster

  attr_accessor :id, :name, :type, :type_name, :affinity, :chaos

  def initialize
    super(nil)
    @id = nil
    @name = ''
    @type = nil
    @type_name = ""
    @affinity = nil
    @chaos = nil
  end

end
