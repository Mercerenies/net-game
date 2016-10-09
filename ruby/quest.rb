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

  def self.from_sxp(arg)
    id, name, nature, specifics = Reloader.assert_first :quest, arg
    Quest.new(id, name, nature).tap do |quest|
      specifics.each_slice(2) { |k, v| quest.add_specifics(k, v) }
    end
  end

end

class ReloadedQuest < Quest

  def initialize(id, name, nature)
    super name, nature
    @id = id
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

  def self.from_sxp(arg)
    arr = Reloader.assert_first :'quest-set', arg
    QuestSet.new.tap do |set|
      Reloader.list_like(arr) { |x| set.push x }
    end
  end

end

module QuestMaker
  @@quest_flag_n = 0

  def self.current_quest_flag
    @@quest_flag_n
  end

  def self.current_quest_flag=(val)
    @@quest_flag_n = val.to_i
  end

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
