
module Questing

  # QuestStub is a tag module for quest stubs, which are classes which should have both of the following methods.
  # * +final_type+ - A 0-ary method which returns the expected return type of +to_final+.
  # * +to_final+ - A method taking a QuestProvider instance and returning an object of the appropriate type,
  #   usually a Quest instance.
  # * +deep_copy+ - A 0-ary method producing a new QuestStub of the same type as the original but with all of
  #   the internals having been deeply copied.
  module QuestStub

=begin Sample implementation

    def final_type
      Quest
    end

    def to_final(provider)
      # Produce a quest instance ...
    end

    def deep_copy
      self.dup.tap do |x|
        # Deeply copy x's fields ...
      end
    end

=end

  end

  # Expandable is a tag module for parts of the quest maker which can be expanded during the search algorithm.
  # The Expandable module expects one method: an +expand+ which takes no arguments and provides a single
  # value of the appropriate type, usually using some random generation to produce the result.
  module Expandable
  end

  class ExpandableOf
    include Expandable

    def initialize(args)
      @ary = args.to_a
    end

    def self.[](*args)
      self.new(args)
    end

    def expand
      @ary.sample
    end

  end

end
