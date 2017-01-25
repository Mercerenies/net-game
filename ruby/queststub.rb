
# QuestStub is a deep-copy-able version of Quest that does not eat up ID values, since it only produces a
# "finalized" quest when asked to do so.
class QuestStub
  attr_reader :name

  def initialize(name)
    self.name = name.to_s
    @states = {}
  end

  def name=(name)
    @name = name.to_s
  end

  def [](state)
    @states[state]
  end

  def []=(state, val)
    @states[state] = val
  end

  def to_quest
    Quest.new(name).tap do |quest|
      @states.each do |k, v|
        quest[k] = v
      end
    end
  end

  def initialize_copy(src)
    @states = Hash[ @states.map { |k, v| [k, v.dup] } ]
  end

end
