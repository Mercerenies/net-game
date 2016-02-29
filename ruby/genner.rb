
class Genner

  def initialize(everything)
    @arr = everything.clone
    @nodes = []
    @options = {}
    @map = nil
  end

  def generate_nodes
    @nodes = []
    @arr = @arr.reject do |elem|
      if elem.is_a? Place
        case elem.type
        when :city then @nodes << generate_node(elem, Level.city)
        when :state then @nodes << generate_node(elem, Level.state)
        when :country then @nodes << generate_node(elem, Level.country)
        when :district then @nodes << generate_node(elem, Level.district)
        when :bank, :tower, :estate, :library, :castle, :forest, :garden then nil
        when :landform, :moon, :museum, :park, :plain, :river then nil
        end
      end
    end
  end

  def generate_node(elem, lvl)
    name, child = if elem.name =~ /([^,]+), ([^,]+)/
                    [$1, $2]
                  else
                    [elem.name, nil]
                  end
    node = Node.new name, lvl
    node.waterfall
    node = node_level_up node
    if child
      if lvl.level_up
        node = Node.new(child, lvl.level_up).tap { |o| o << node }
      else
        children = lvl.make_children
        unless children.empty?
          other = Node.new(node.name, children[0])
          other.waterfall
          node.name = child
          node << other
        end
      end
    end
    node
  end

  def generate_map
    temp = Node.new '', Level.individual
    @nodes.each { |obj| temp << obj }
    @map = Map.new temp.expand_to_map
  end

  def generate_buildings
    buildings = []
    @arr = @arr.reject do |elem|
      if elem.kind_of? Place
        building = Building.load_building elem
        buildings << building if building
      end
    end
    buildings.each do |building|
      curr = @map.to_ary.sample
      building.each_exit do |exit|
        curr = @map[ curr.each_link.to_a.sample ]
        exit.add_link curr.id
        curr.add_link exit.id
      end
      building.each_node { |x| @map.push x }
    end
  end

  def generate
    # Stage 1 - Generate the main nodal structures
    generate_nodes
    # Stage 2 - Convert the nodes into a map
    generate_map
    # Stage 3 - Add buildings to the map
    generate_buildings
    # Return result
    @map
  end

  private :generate_nodes, :generate_node, :generate_map, :generate_buildings

end
