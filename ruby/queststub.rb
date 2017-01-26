
module Questing
  include QuestBuilder

  # QuestStub is a tag module for quest stubs, which are classes which should have both of the following methods.
  # * +final_type+ - A 0-ary method which returns the expected return type of +to_final+.
  # * +to_final+ - A method taking a QuestProvider instance and returning an object of the appropriate type,
  #   usually a Quest instance.
  module QuestStub

=begin Sample implementation

    def final_type
      Quest
    end

    def to_final(provider)
      # Produce a quest instance ...
    end

=end

  end

  # Expandable is a tag module for parts of the quest maker which can be expanded during the search algorithm.
  # The Expandable module expects two methods: an +expand+ which takes no arguments and provides a single
  # value of the appropriate type and an +expanded?+ method which returns whether the stub (and all of its
  # children) has been fully expanded.
  module Expandable

    # Often, the default implementation of +expanded?+, which simply returns false, is sufficient for simple
    # cases. It will be necessary to override this method when the Expandable object is complex enough to
    # contain other, potentially Expandable elements, in which case this method should call +expanded?+
    # on those elements for which it makes sense to do so.
    def expanded?
      false
    end

  end

  # A simple Expandable object which stores a list of elements that it could potentially expand into.
  class ExpandableOf
    include Expandable

    def initialize(args)
      @ary = args.to_a
    end

    def self.[](*args)
      self.new(args)
    end

    # Expands the ExpandableOf object into one of the elements in the list given at initialization
    # time, chosen randomly with a uniform distribution.
    def expand
      @ary.sample
    end

  end

end
