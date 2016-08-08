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

  def_delegators :@page, :name, :pack, :speed, :threat, :size

  def initialize(data)
    super()
    @page = data
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

end
