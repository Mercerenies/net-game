require 'forwardable'

class CreatureSet
  include Enumerable

  def initialize
    @animals = []
  end

  # Returns whether it was able to successfully load data from the page
  def load_from_page(x)
    case x
    when AnimalPage
      push Animal.new(x)
    end
  end

  def push(x)
    case x
    when Animal
      @animals << x
      true
    end
    false
  end

  def each(&block)
    each_animal &block
  end

  def each_animal(&block)
    @animals.each &block
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
    @animals.empty?
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

  def_delegators :@page, :pack, :speed, :threat, :size

  def initialize(data)
    super()
    @page = data
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
     :':size', size, :':air', air?, :':sea', sea?].to_sxp
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
        end
      end
    end
  end

end

class ReloadedAnimal < Animal
  extend Forwardable

  attr_accessor :id, :name, :pack, :speed, :threat, :size, :air, :sea

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
