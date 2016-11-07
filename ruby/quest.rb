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
