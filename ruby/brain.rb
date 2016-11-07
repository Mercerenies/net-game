
require 'forwardable'

# For efficiency reasons, it is undesirable to iterate over all of the locations in the world map. Thus,
# when it is necessary to search for a specific person, the knowledge base is searched. A KnowledgeBase
# instance stores a collection of NPCBrain objects.
class KnowledgeBase
  extend Forwardable

  attr_reader :data

  def initialize
    @data = {}
  end

  # Iterates over the entries in the knowledge base.
  #  kb.each { |id, brain| puts id }
  def each(&block)
    @data.each(&block)
  end

  # Accesses the brain of the given person object, constructing an empty brain if it does
  # not exist.
  def [](person)
    unless @data.include? person
      @data[person.id] = NPCBrain.new(person.id, person.name, person.job)
    end
    @data[person.id]
  end

  # Assign the brain for the given person object.
  def []=(key, value)
    @data[key.id] = value
  end

  # Add an empty brain to the given person. If the person already has a brain in this knowledge base,
  # this method will not replace it.
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

# A single brain for an NPC. The brain instance keeps track of the NPC's basic information, as well as
# any quests that that NPC initiates.
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

  # Adds the quest identifier given to the NPC's quest list
  def add_quest(q)
    @quests.push q
  end

  # Returns the number of quests the NPC initiates.
  def quest_count
    each.to_a.size
  end

  # Returns whether the NPC is responsible for initiating any quests.
  def has_quests?
    quest_count > 0
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :'npc-brain', arg
    NPCBrain.new(0, "", nil).tap do |brain|
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
