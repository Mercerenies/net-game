require 'forwardable'

class Spawner

  def initialize(creature, map, center, radius)
    @nodes = []
    @creature = creature
    spread_from_center map, center, radius
  end

  def spread_from_center(map, node, radius)
    if radius > 0 and not @nodes.include? node
      @nodes << node if node.can_have? @creature
      node.each_link do |link|
        spread_from_center map, map[link], radius - 1
      end
    end
  end

  def node_ids
    @nodes.collect(&:id).each
  end

  def creature_id
    @creature.id
  end

  def to_sxp
    prefix = [:spawner]
    creature = [:':creature', creature_id]
    area = [:':area', node_ids.to_a]
    (prefix + creature + area).to_sxp
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :spawner, arg
    ReloadedSpawner.new.tap do |spawn|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':creature'
          spawn.creature_id = v
        when :':area'
          spawn.node_ids = v
        end
      end
    end
  end

  private :spread_from_center
end

class ReloadedSpawner < Spawner
  attr_accessor :node_ids, :creature_id

  def initialize
    @node_ids = []
    @creature_id = nil
  end

end

class SpawnerSet
  include Enumerable

  def initialize
    @spawners = []
  end

  def push(x)
    case x
    when Spawner
      @spawners << x
      true
    end
    false
  end

  def each(&block)
    each_spawner &block
  end

  def each_spawner(&block)
    @spawners.each &block
  end

  def to_sxp
    ([:'spawner-set'] + to_a).to_sxp
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :'spawner-set', arg
    SpawnerSet.new.tap do |set|
      Reloader.list_like(arr) { |x| set.push x }
    end
  end

  def empty?
    @spawners.empty?
  end

end
