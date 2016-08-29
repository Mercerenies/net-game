
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

  def make_default_node
    new_country = PlacePage.new({
                                  'name' => Namer.instance.sample,
                                  'info' => ['country', 'country']
                                })
    generate_node(new_country, Level.country)
  end

  def run(data)
    nodes = []
    data.consume_each do |elem|
      if elem.kind_of? PlacePage
        case elem.type
        when :city then nodes << generate_node(elem, Level.city)
        when :state then nodes << generate_node(elem, Level.state)
        when :country then nodes << generate_node(elem, Level.country)
        when :district then nodes << generate_node(elem, Level.district)
        when :bank, :tower, :estate, :library, :castle, :forest, :garden then nil
        when :landform, :moon, :museum, :park, :plain, :river, :village then nil
        when :street, :waterfall then nil
        end
      end
    end
    if nodes.length < 2
      default = make_default_node
      nodes << default if default
    end
    toplevel = Node.new '', Level.top
    nodes.each { |obj| toplevel << obj }
    data.node = toplevel
  end

end

class DeltaNodeStage < NodeStage
  def make_default_node
    nil
  end
end

# Stage 2 - Pre-generate any bridges that can be made
class BridgeStage < Stage
  def run(data)
    bridges = []
    data.consume_each do |elem|
      if elem.kind_of? PlacePage
        bridge = Bridge.load_bridge elem
        bridges << bridge if bridge
      end
    end
    data.add_bridges(*bridges)
  end
end

# Stage 3 - Convert the nodes into a map
class MapStage < Stage
  def run(data)
    data.node_to_map
  end
end

# Stage 4 - Add buildings to the map
class BuildingStage < Stage
  def run(data)
    buildings = []
    data.consume_each do |elem|
      if elem.kind_of? PlacePage
        building = Building.load_building elem
        buildings << building if building
      end
    end
    buildings.each do |building|
      building.integrate_with data.map
      building.each_node { |x| data.map.push x }
    end
  end
end

# Stage 5 - Make a list of creatures and put them places
class CreatureStage < Stage
  def run(data)
    # Identify and set up valid creatures
    data.consume_each { |elem| data.load_creature elem }
    creatures = data.each_creature.to_a.shuffle.cycle
    return unless data.has_creature?
    # Now identify all of the "dangerous" nodes, that is
    # anywhere that should have a creature in it
    needed = data.select_nodes(&:can_have_creatures?).to_a
    # Now put animals / creatures in those spots
    until needed.empty?
      spawner = Spawner.new creatures.next, data.map, needed.first, [2, 2, 2, 3].sample
      area = spawner.node_ids.map { |id| data.map[id] }
      needed = needed.reject { |loc| area.include? loc }
      data.add_spawners spawner
    end
  end
end

# Stage 6 - Put items into the map
class ItemStage < Stage
  def run(data)
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
    foods = CriteriaQueue[]
    data.consume_each do |elem|
      case elem
      when FoodPage
        foods << Food.new(elem) if elem.plant
      end
    end
    data.map.each do |loc|
      current_count = loc.count_items Plant
      if rand < 0.90 / (current_count + 1)
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

# Stage 9 - Put default quests for any person who lacks quests
class QuestStage < Stage
  def run(data)
    data.map.each do |node|
      node.each do |obj|
        case obj
        when NPC
          data.add_quests QuestMaker.make_fetch_quest(data.map, obj)
        end
      end
    end
  end
end

# Stage 10 - Position the player
class PlayerStage < Stage
  def run(data)
    data.map.put_somewhere Player.new
  end
end
