
require 'forwardable'

# For efficiency reasons, it is undesirable to iterate over all of the locations in the world map.
# Thus, when it is necessary to search for a specific person, the knowledge base is searched. A
# KnowledgeBase instance stores a collection of NPCBrain objects.
class KnowledgeBase
  include DeltaAble

  attr_reader :data

  def initialize
    @data = {}
  end

  def to_delta
    DeltaKnowledgeBase.new self
  end

  # Iterates over the entries in the knowledge base.
  #  kb.each { |id, brain| puts id }
  def each(&block)
    @data.each(&block)
  end

  # Accesses the brain of the given person object, constructing an empty brain if it does
  # not exist.
  def [](key)
    unless @data.include? key.id
      @data[key.id] = KnowledgeBase.new_brain key
    end
    @data[key.id]
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
    arr = to_h.values
    ([:'knowledge-base'] + arr).to_sxp
  end

  def to_h
    data
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :'knowledge-base', arg
    KnowledgeBase.new.tap do |kb|
      Reloader.list_like(arr) { |v| kb.data[v.id] = v }
    end
  end

  def self.new_brain(key)
    case key
    when NPC
      NPCBrain.new key.id, key.name, key.job
    when Location
      CityBrain.new key.id
    end
  end

end

# A single brain for an NPC. The brain instance keeps track of the NPC's basic information, as well as
# any quests that that NPC initiates.
class NPCBrain
  attr_reader :id, :name, :job, :motives

  def initialize(id, name, job)
    @id = id
    @name = name
    @job = job
    @quests = []
    @motives = Motivation.motive_for job
  end

  def to_delta
    DeltaNPCBrain.new self
  end

  def each(&block)
    @quests.each(&block)
  end

  def to_sxp # TODO Why are name and job in meta? Maybe we should move them into the main data
    meta = MetaData.new(:':id' => id, :':job' => job, :':name' => name)
    [:'npc-brain', id, :':quests', each.to_a, :':motives', motives, :':meta', meta].to_sxp
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
    id_, *arr = Reloader.assert_first :'npc-brain', arg
    ReloadedNPCBrain.new(id_, "", nil).tap do |brain|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':quests'
          v.each { |n| brain.add_quest n }
        when :':motives'
          brain.motives = Reloader.load v
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

class ReloadedNPCBrain < NPCBrain
  attr_writer :id, :name, :job, :motives
end

class CityBrain
  attr_reader :id

  def initialize(id)
    @id = id
  end

  def to_sxp
    [:'city-brain', id].to_sxp
  end

  def self.from_sxp(arg)
    id_, *ignore = Reloader.assert_first :'city-brain', arg
    CityBrain.new id_
  end

end
