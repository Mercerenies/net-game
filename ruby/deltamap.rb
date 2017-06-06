
class DeltaMap < Map
  include Delta

  def initialize(map)
    @old_ary = map.collect { |x| DeltaLocation.new x }
    @new_ary = []
  end

  def each(&block)
    if block.nil?
      Enumerator.new do |y|
        @old_ary.each { |obj| y << obj }
        @new_ary.each { |obj| y << obj }
      end
    else
      @old_ary.each(&block)
      @new_ary.each(&block)
    end
  end

  def each_new_node(&block)
    @new_ary.each(&block)
  end

  def push(*args)
    @new_ary.push(*args)
  end

  def to_ary
    @old_ary + @new_ary
  end

  def to_dsxp
    # In the delta map, newly added nodes can be modified freely. Old nodes
    # should only be modified to the extent allowed by DeltaLocation.
    [:map, :':new', @new_ary, :':mod', @old_ary.map(&:to_dsxp)]
  end

end

class DeltaLocation < Location
  include Delta
  extend Forwardable

  def_delegators :@new_contents, :push
  def_delegators :@old_node, :id, :name, :country_name, :generic_name,
                             :can_have_creatures?, :can_have?, :valid_creatures,
                             :valid_plants, :water_mode, :mark_as_dry,
                             :mark_as_sea, :mark_as_shore, :fitness

  def initialize(old)
    @old_node = old
    @new_contents = []
    @new_links = []
    @sans_links = []
    @new_linkage = old.linkage
  end

  def add_link(x)
    if @old_node.each_link.include? x and @sans_links.include? x
      @sans_link.delete x
    elsif not @new_links.include? x
      @new_links.push x
    end
  end

  def remove_link(x)
    if @new_links.include? x
      @new_links.delete x
    elsif @links.include? x
      @sans_links.push x
    end
  end

  def each_link(&block)
    if block.nil?
      Enumerator.new do |y|
        @old_node.each_link.reject { |obj| @sans_links.include? obj }.each { |obj| y << obj }
        @new_links.each { |obj| y << obj }
      end
    else
      @old_node.each_link.reject { |obj| @sans_links.include? obj }.each(&block)
      @new_links.each(&block)
    end
  end

  def each(&block)
    if block.nil?
      Enumerator.new do |y|
        @old_node.each { |obj| y << obj }
        @new_contents.each { |obj| y << obj }
      end
    else
      @old_node.each(&block)
      @new_contents.each(&block)
    end
  end

  def delete(x, &block)
    if @new_contents.include? x
      @new_contents.delete x
    elsif @old_node.include? x
      raise DeltaError, "Attempt to delete read-only contents of DeltaLocation"
    else
      block.call
    end
  end

  def linkage
    @new_linkage
  end

  def linkage=(x)
    # TODO Error checking, like in map.rb for the same method
    @new_linkage = x
  end

  def to_dsxp
    # This is the delta interface to old, reloaded nodes. The following rules
    # should be followed when modifying delta nodes.
    #  - Links can be added and removed freely, even pre-existing links.
    #  - New objects can be added. Old objects should not be removed or modified
    #    in any way.
    #  - Basic node properties, such as name and allowable creatures, are
    #    read-only.
    [:location, id,
     :':remove-links', @sans_links,
     :':add-links', @new_links,
     :':add-contents', @new_contents]
  end

end
