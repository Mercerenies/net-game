
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

# A NavLocationLens is a navigation lens examining zero or more locations. From this lens,
# methods are provided to move to nearby locations and to seek out the contents of the
# lens.
class NavLocationLens

  def initialize(nav, arr)
    @nav = nav
    @locs = arr
  end

  # Returns a new NavLocationLens examining all nodes which are adjacent to at least one of
  # the nodes in the current lens. Note that #surrounding will return all adjacent nodes in
  # addition to the currently selected ones, while #adjacent only returns adjacent nodes.
  def adjacent
    arr = @locs.flat_map { |x| x.each_link.to_a }.flat_map { |x| @nav.map[x] }
    NavLocationLens.new @nav, arr.to_a
  end

  # Returns a new NavLocationLens examining all nodes which are equal or adjacent to at
  # least one of the nodes in the current lens. Note that #surrounding will return all
  # adjacent nodes in addition to the currently selected ones, while #adjacent only
  # returns adjacent nodes.
  def surrounding
    arr = @locs.flat_map { |x| x.each_link.to_a }.flat_map { |x| @nav.map[x] }
    NavLocationLens.new @nav, (self.to_a + arr.to_a)
  end

  # Returns a new NavLocationLens examining all nodes which are at most +n+ links away from
  # the current node. Note that calling #within with an argument of +0+ will return a duplicate
  # of the current lens, and calling #within with an argument of +1+ is equivalent to calling
  # #surrounding.
  def within(n)
    if n < 0
      NavLocationLens.new @nav, []
    elsif n == 0
      self
    else
      self.surrounding.within(n - 1).prune
    end
  end

  # Returns a pruned version of the lens. It is not normally to do this explicitly unless
  # the lens constructor is being explicitly called. All of the built-in lens operations
  # automatically prune where necessary.
  def prune
    NavLocationLens.new @nav, @locs.uniq
  end

  # Returns a NavContentsLens containing all of the objects in the selected locations.
  def contents
    objs = @locs.flat_map { |x| x.each.to_a }
    NavContentsLens.new @nav, objs
  end

  # Returns the array of nodes that are being examined with this lens.
  def to_a
    @locs.dup
  end

end

# A NavContentsLens, usually constructed by NavLocationLens#contents, examines a collection
# of objects at zero or more locations.
class NavContentsLens

  def initialize(nav, arr)
    @nav = nav
    @objs = arr
  end

  # Returns a pruned version of the lens. It is not normally necessary to call this method
  # explicitly, as the built-in lens operations prune where necessary by default.
  def prune
    NavContentsLens.new @nav, @objs.uniq
  end

  # Detects the first of the selected objects for which the block returns truthy.
  # If such an object exists, it is returned. Otherwise, +nil+ is returned by default.
  def detect(&block)
    @objs.detect &block
  end

  # Detects the first of the selected objects which matches (according to #===) the
  # argument object. If such an object exists, it is returned. Otherwise, +nil+ is
  # returned.
  def has?(obj)
    detect { |x| case x when obj then x end }
  end

  # Returns an array of the objects being examined with this lens.
  def to_a
    @objs.dup
  end

end

# An exception for errors in the integrity of the navigator.
class NavigatorError < StandardError

  def initialize(msg = "Error navigating the map")
    super
  end

end
