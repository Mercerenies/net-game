require 'forwardable'

class Quest
  attr_reader :id, :name, :nature, :specifics

  def initialize(name, nature)
    @id = Node.get_id
    @name = name
    @nature = nature
    @specifics = []
  end

  def add_specifics(key, value)
    @specifics.unshift key, value
    self # Returns self so calls can be chained
  end

  def to_sxp
    [:quest, id, name, nature, specifics].to_sxp
  end

end

class QuestSet
  include Enumerable
  extend Forwardable

  def_delegators :@quests, :each, :push, :empty?

  def initialize
    @quests = []
  end

  def to_sxp
    ([:'quest-set'] + to_a).to_sxp
  end

end

module QuestMaker
  @@quest_flag_n = 0

  def self.make_quest_flag
    @@quest_flag_n += 1
    ("qf" + @@quest_flag_n.to_s.rjust(4, '0')).intern
  end

  def self.make_fetch_quest(map, person)
    flag = QuestMaker.make_quest_flag
    item_raw_name = nil
    item = Item.make_random do |name|
      item_raw_name = name
      "#{person.name}'s #{item_raw_name}"
    end
    item.add_flags flag
    item_loc = map.put_somewhere item
    Quest.new("#{person.name}'s Missing #{item_raw_name}", :fetch).tap do |q|
      person.add_quest q.id
      q.add_specifics :'item-flag', flag
      q.add_specifics :'item-name', item_raw_name
      q.add_specifics :'item-loc', item_loc.generic_name
    end
  end

end
