
require 'forwardable'

class KnowledgeBase
  extend Forwardable

  def_delegator :@data, :each, :each

  def initialize
    @data = {}
  end

  def [](key)
    @data[key] = NPCBrain.new unless @data.include? key
    @data[key]
  end

  def []=(key, value)
    @data[key] = value
  end

  def to_sxp
    arr = each.to_a.flatten 1
    ([:'knowledge-base'] + arr).to_sxp
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :'knowledge-base', arg
    KnowledgeBase.new.tap do |kb|
      Reloader.hash_like(arr) { |k, v| kb[k] = Reloader.instance.load v }
    end
  end

end

class NPCBrain

  def initialize
    @quests = []
  end

  def each(&block)
    @quests.each(&block)
  end

  def to_sxp
    ([:'npc-brain'] + each.to_a).to_sxp
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
    NPCBrain.new.tap do |brain|
      arr.each { |n| brain.add_quest n }
    end
  end

end
