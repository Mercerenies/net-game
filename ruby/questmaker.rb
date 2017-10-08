
module QuestMaker
  include QuestBuilder

  @@quest_flag_n = 0

  def self.current_quest_flag
    @@quest_flag_n
  end

  def self.current_quest_flag=(val)
    @@quest_flag_n = val.to_i
  end

  def self.make_quest_flag
    @@quest_flag_n += 1
    # TODO Shall we turn this into a hierarchical list flag?
    ("qf-rb-" + @@quest_flag_n.to_s.rjust(4, '0')).intern
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
      q[1] = [trigger(talk_to(brain, "Your #{item_raw_name.downcase}?")) {
                if_has_item([:flag, flag]) {
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

# A provider of ID values and flags for quest instances. Some of the methods in this class delegate to the
# QuestMaker module, but QuestProvider provides some additional, localized functionality that is not available
# globally.
class QuestProvider

  def initialize
    @numbers = 0
    @prefixes = {}
  end

  def quest_empty_stage
    0
  end

  def quest_finished_state
    :completed
  end

  def make_quest_flag
    QuestMaker.make_quest_flag
  end

  def make_quest_state(prefix = nil)
    if prefix
      key = prefix.to_s.upcase
      if @prefixes.include? key
        @prefixes[key] += 1
      else
        @prefixes[key] = 1
      end
      (key + @prefixes[key].to_s).intern
    else
      @numbers += 1
      @numbers
    end
  end

end
