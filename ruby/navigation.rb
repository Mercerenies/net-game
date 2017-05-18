
# Navigator is a thin wrapper around a Map instance which provides functionality for moving about on
# the map and examining specific nodes.
class Navigator
  attr_reader :map

  def initialize(map)
    @map = map
  end

  # Returns a navigation object centered around a specific location. Note that +loc+ should be
  # a Location instance which belongs to the navigator's map, or an ID corresponding to a
  # Location instance in the map.
  def node(loc)
    # This looks a little bit redundant, but it does its job. If you pass in an ID, loc1 is a
    # no-op and loc2 retrieves the location. If you pass in a Location, loc1/loc2 cancel each
    # other off but will check if the location node is actually in the map.
    loc1 = if loc.is_a? Location then loc.id else loc end
    loc2 = @map[loc1]
    unless loc2
      raise NavigatorError, "Invalid location to Navigator#node"
    end
    NavLocationLens.new self, [loc2]
  end

end

class NavLocationLens

  def initialize(nav, arr)
    @nav = nav
    @locs = arr
  end

  def adjacent
    arr = @locs.flat_map { |x| x.each_link.to_a }.flat_map { |x| @nav.map[x] }
    NavLocationLens.new @nav, arr.to_a
  end

  def surrounding
    arr = @locs.flat_map { |x| x.each_link.to_a }.flat_map { |x| @nav.map[x] }
    NavLocationLens.new @nav, (self.to_a + arr.to_a)
  end

  def within(n)
    if n < 0
      NavLocationLens.new @nav, []
    elsif n == 0
      self
    else
      self.surrounding.within(n - 1).prune
    end
  end

  def prune
    NavLocationLens.new @nav, @locs.uniq
  end

  def contents
    objs = @locs.flat_map { |x| x.each.to_a }
    NavContentsLens.new @nav, objs
  end

  def to_a
    @locs.dup
  end

end

class NavContentsLens

  def initialize(nav, arr)
    @nav = nav
    @objs = arr
  end

  def prune
    NavContentsLens.new @nav, @objs.uniq
  end

  def to_a
    @objs.dup
  end

end

# An exception for errors in the integrity of the navigator
class NavigatorError < StandardError
  def initialize(msg = "Error navigating the map")
    super
  end
end
