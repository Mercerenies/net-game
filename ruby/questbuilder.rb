
module QuestBuilder

  class QuestTrueFalse

    def initialize
      @truthy = nil
      @falsey = nil
    end

    def when_true(&block)
      qt = QuestTriggerScope.new
      qt.progn &block
      @truthy = qt.first
    end

    def when_false(&block)
      qt = QuestTriggerScope.new
      qt.progn &block
      @falsey = qt.first
    end

    def if_true(&block)
      when_true(&block)
    end

    def if_false(&block)
      when_false(&block)
    end

    def unless_true(&block)
      when_false(&block)
    end

    def unless_false(&block)
      when_true(&block)
    end

    def block(&block)
      self.instance_eval(&block)
    end

    def finish
      [@truthy, @falsey]
    end

  end

  class QuestBranchScope

    def initialize
      @arr = []
    end

    def block(&block)
      self.instance_eval(&block)
    end

    def choice(text, &block)
      tr = QuestTriggerScope.new
      tr.progn(&block)
      @arr.push text, tr.first
    end

    def finish
      @arr
    end

  end

  class QuestTriggerScope

    def initialize
      @arr = []
    end

    def block(&block)
      self.instance_eval(&block)
    end

    def progn(&block)
      qt = QuestTriggerScope.new
      qt.block &block
      @arr.push([:begin] + qt.finish)
    end

    def goto(state)
      @arr.push [:goto, state]
    end

    def complete
      @arr.push [:complete]
    end

    def accept(state)
      @arr.push [:accept, state]
    end

    def speak(text)
      @arr.push [:speak, text]
    end

    def branch(prompt, &block)
      br = QuestBranchScope.new
      br.block(&block)
      @arr.push([:branch, prompt] + br.finish)
    end

    def if_has_item(match, &block)
      tf = QuestTrueFalse.new
      tf.block(&block)
      @arr.push([:'if-has-item', match] + tf.finish)
    end

    def remove_item(match)
      @arr.push [:'remove-item', match]
    end

    def finish
      @arr
    end

    def first
      finish[0]
    end

  end

  def trigger(k, &block)
    qt = QuestTriggerScope.new
    qt.block &block
    ([k] + qt.finish)
  end

  def initiate
    :initiate
  end

  def talk_to(brain, prompt)
    [:'talk-to', brain.id, prompt]
  end

  def self.included(base)
    base.extend self
  end

end
