
class Stage
  def run(data)
  end
end

# Stage 1 - Generate the main nodal structures
class NodeStage < Stage

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

  def run(data)
    STDERR.puts "Stage 1"
    nodes = []
    data.consume_each do |elem|
      if elem.kind_of? PlacePage
        case elem.type
        when :city then nodes << generate_node(elem, Level.city)
        when :state then nodes << generate_node(elem, Level.state)
        when :country then nodes << generate_node(elem, Level.country)
        when :district then nodes << generate_node(elem, Level.district)
        when :bank, :tower, :estate, :library, :castle, :forest, :garden then nil
        when :landform, :moon, :museum, :park, :plain, :river then nil
        end
      end
    end
    data.nodes = nodes
  end

end

# Stage 2 - Pre-generate any bridges that can be made
class BridgeStage < Stage
  def run(data)
    STDERR.puts "Stage 2"
    bridges = []
    data.consume_each do |elem|
      if elem.kind_of? PlacePage
        bridge = Bridge.load_bridge elem
        bridges << bridge if bridge
      end
    end
    data.bridges = bridges
  end
end

# Stage 3 - Convert the nodes into a map
class MapStage < Stage
  def run(data)
    STDERR.puts "Stage 3"
    temp = Node.new '', Level.individual
    data.nodes.each { |obj| temp << obj }
    data.map = Map.new temp.expand_to_map gdata: data
  end
end

# Stage 4 - Add buildings to the map
class BuildingStage < Stage
  def run(data)
    STDERR.puts "Stage 4"
    buildings = []
    data.consume_each do |elem|
      if elem.kind_of? PlacePage
        building = Building.load_building elem
        buildings << building if building
      end
    end
    buildings.each do |building|
      building.each_node { |x| data.map.push x }
      building.integrate_with data.map
    end
  end
end

# Stage 5 - Make a list of creatures and put them places
class CreatureStage < Stage
  def run(data)
    STDERR.puts "Stage 5"
    # Identify and set up valid creatures
    data.consume_each { |elem| data.load_creature elem }
    creatures = data.creatures.to_a.shuffle.cycle
    return unless data.has_creature?
    # Now identify all of the "dangerous" nodes, that is
    # anywhere that should have a creature in it
    needed = data.select_nodes(&:can_have_creatures?).to_a
    # Now put animals / creatures in those spots
    until needed.empty?
      spawner = Spawner.new creatures.next, data.map, needed.first, [2, 2, 2, 3].sample
      area = spawner.area_covered.to_a
      needed = needed.reject { |loc| area.include? loc }
      data.spawners.push spawner
    end
  end
end

# Stage 6 - Put items into the map
class ItemStage < Stage
  def run(data)
    STDERR.puts "Stage 6"
    data.consume_each do |elem|
      case elem
      when WeaponPage
        data.map.put_somewhere(Weapon.new elem.name, elem.type) if elem.type
      end
    end
  end
end

# Stage 7 - Put plants that grow food on the map
class FoodStage < Stage
  def run(data)
    STDERR.puts "Stage 7"
    foods = CriteriaQueue[]
    data.consume_each do |elem|
      case elem
      when FoodPage
        foods << Food.new(elem) if elem.plant
      end
    end
    data.map.each do |loc|
      if rand < 0.90
        # Try to put some sort of food at the location, if possible
        food = foods.shift { |f| loc.can_have? f }
        if food
          loc.push Plant.new(food.plant_type, food.dup)
          foods << food # We want to leave the food at the back of the queue for later
        end
      end
    end
  end
end

# Stage 8 - Make people and put them somewhere
class PersonStage < Stage
  def run(data)
    STDERR.puts "Stage 8"
    data.consume_each do |elem|
      case elem
      when PersonPage
        if elem.gender and not elem.occupations.empty?
          data.map.put_somewhere(NPC.new elem) { |loc| loc.civilized? }
        end
      end
    end
  end
end

# Stage 9 - Position the player
class PlayerStage < Stage
  def run(data)
    STDERR.puts "Stage 9"
    data.map.put_somewhere Player.new
  end
end
