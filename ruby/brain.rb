
require 'forwardable'

class KnowledgeBase
  extend Forwardable

  attr_reader :data
  def_delegator :@data, :each, :each

  def initialize
    @data = {}
  end

  def [](person)
    @data[person.id] = NPCBrain.new(person.id, person.name, person.job) unless @data.include? person
    @data[person.id]
  end

  def []=(key, value)
    @data[key.id] = value
  end

  def add_empty(person)
    self[person] # Referencing the field forces the creation, according to the accessor above
  end

  def to_sxp
    arr = each.to_a.flatten 1
    ([:'knowledge-base'] + arr).to_sxp
  end

  def to_h
    data
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :'knowledge-base', arg
    KnowledgeBase.new.tap do |kb|
      Reloader.hash_like(arr) { |k, v| kb.data[k] = Reloader.instance.load v }
    end
  end

end

class NPCBrain
  attr_accessor :id, :name, :job # TODO Move this accessor to a ReloadedNPCBrain child

  def initialize(id, name, job)
    @id = id
    @name = name
    @job = job
    @quests = []
  end

  def each(&block)
    @quests.each(&block)
  end

  def to_sxp
    meta = MetaData.new(:':id' => id, :':job' => job, :':name' => name)
    [:'npc-brain', :':quests', each.to_a, :':meta', meta].to_sxp
  end

  def add_quest(q) # Expects a quest identifier
    @quests.push q
  end

  def quest_count
    each.to_a.size
  end

  def has_quests?
    quest_count > 0
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :'npc-brain', arg
    NPCBrain.new(nil).tap do |brain|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':quests'
          v.each { |n| brain.add_quest n }
        when :':meta'
          meta = Reloader.load v
          brain.job = meta[:':job']
          brain.name = meta[:':name']
          brain.id = meta[:':id']
        end
      end
    end
  end

end
