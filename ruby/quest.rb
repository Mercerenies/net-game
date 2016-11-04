require 'forwardable'

class Quest
  attr_reader :id, :name

  def initialize(name)
    @id = Node.get_id
    @name = name
    @states = {}
  end

  def [](state)
    @states[state]
  end

  def []=(state, val)
    @states[state] = val
  end

  def to_sxp
    states = @states.flat_map { |k, v| [k, v] }
    [:quest, id, name, :':states', states].to_sxp
  end

  def self.from_sxp(arg)
    id, name, *specifics = Reloader.assert_first :quest, arg
    ReloadedQuest.new(id, name).tap do |quest|
      Reloader.hash_like(specifics) do |k, v|
        case k
        when :':states'
          quest[k] = v
        end
      end
    end
  end

end

class ReloadedQuest < Quest

  def initialize(id, name)
    super name
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
  extend QuestBuilder

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

  def self.make_fetch_quest(map, brain)
    flag = QuestMaker.make_quest_flag
    item_raw_name = nil
    item = Item.make_random do |name|
      item_raw_name = name
      "#{brain.name}'s #{item_raw_name}"
    end
    item.add_flags flag
    item_loc = map.put_somewhere item
    Quest.new("#{brain.name}'s Missing #{item_raw_name}").tap do |q|
      q[0] = [trigger(initiate) {
                branch("Hey! You seem fairly capable. I think I dropped my #{item_raw_name.downcase} somewhere. Do you think you could go and get it?") {
                  choice("Sure thing!") {
                    accept 1
                    speak "Perfect! I'm pretty sure I left it somewhere near #{item_loc.generic_name}."
                  }
                  choice("I don't have time.") {
                    speak "Oh... sorry to bother you."
                  }
                }
              }]
      q[1] = [trigger(talk_to(brain, "Your item?")) {
                if_has_item(flag) {
                  if_true {
                    complete
                    remove_item flag
                    speak "Oh, my #{item_raw_name.downcase}! Thank you so much!"
                  }
                  if_false {
                    speak "Remember. You're looking for my #{item_raw_name.downcase} near #{item_loc.generic_name}."
                  }
                }
              }]
      q[:completed] = []
    end
  end

end
