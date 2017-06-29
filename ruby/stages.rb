
# TODO The "paint splotch" algorithm for spawners and trees creates an uneven distribution when
# making delta files.

# TODO We are generating some extra, unreachable nodes with empty city names somewhere in here,
# and I don't know where. Good luck...

# A stage of the generation process. The Stage class itself is intended as an abstract base class.
class Stage

  # Returns the name of the stage. This name shall only be displayed in debug logs; it shan't be
  # exposed to the user.
  def name
    self.class.name
  end

  # Runs this stage of the generator process, given a GData instance. This should always be
  # overriden by child classes.
  def run(data)
  end

end

# \Stage 1 of the generation process generates the main nodal structure.
class NodeStage < Stage

  def generate_node(elem, lvl)
    # TODO Review this at some point; it might be doing some redundant work that makes the map too big
    name, child = if elem.name =~ /([^,]+), ([^,]+)/
                    [$1, $2]
                  else
                    [elem.name, nil]
                  end
    node = Node.new name, lvl
    node.waterfall
    node = Node.node_level_up node
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

  def make_default_node(data)
    new_country = PlacePage.new({
                                  'name' => Namer.instance.sample,
                                  'info' => ['country', 'country']
                                })
    if data.small_world?
      generate_node new_country, LevelTwo.new(2)
    else
      generate_node new_country, Level.country
    end
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
        when :bank, :tower, :estate then nil
        when :library, :castle, :forest, :garden then nil
        when :landform, :moon, :museum, :park then nil
        when :plain, :river, :village then nil
        when :street, :waterfall then nil
        end
      end
    end
    if nodes.length < 2
      default = make_default_node(data)
      nodes << default if default
    end
    toplevel = Node.new '', Level.top
    nodes.each { |obj| toplevel << obj }
    data.node = toplevel
  end

end

# The first stage of the delta generation process is identical to the
# standard NodeStage except that the default node is not generated.
class DeltaNodeStage < NodeStage
  def make_default_node(data)
    nil
  end
end

# \Stage 2 of the generation process pre-generates any bridges that can be made
# from the data.
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

# \Stage 3 converts the nodal structure generated in NodeStage into a coherent
# map.
class MapStage < Stage
  def run(data)
    data.node_to_map
  end
end

# \Stage 4 adds buildings to the map generated in MapStage.
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

# \Stage 5 constructs a list of creatures and puts generators for them in places
# on the map.
class CreatureStage < Stage
  def run(data)
    halo_size = 3
    nav = data.map.navigator
    # Identify and set up valid creatures
    data.consume_each { |elem| data.load_creature elem }
    all_creatures = data.each_creature.to_a
    creatures = all_creatures.shuffle.cycle
    return unless data.has_creature?
    # Now identify all of the "dangerous" nodes, that is
    # anywhere that should have a creature in it
    needed = data.select_nodes(&:can_have_creatures?).to_a # TODO Efficiency here...?
    # Now put animals / creatures in those spots
    until needed.empty?
      node = needed.first
      if all_creatures.select { |x| node.can_have? x }.empty?
        # If none of the creatures will agree with the node, don't bother
        # looking for one
        needed.delete node
        next
      end
      if nav.node(node).within(halo_size).contents.has?(NeoSpawner)
        # If the node already has a spawner nearby, skip over it
        # TODO With exponentially decreasing probability, go ahead and place one here anyway
        needed.delete node
        next
      end
      creature = creatures.next until node.can_have? creature
      node.push NeoSpawner.new(creature.id)
      needed.delete node
      #spawner = Spawner.new creatures.next, data.map, node, [2, 2, 2, 3].sample
      #area = spawner.node_ids.map { |id| data.map[id] }
      #needed = needed.reject { |loc| area.include? loc }
      #data.add_spawners spawner
    end
  end
end

# \Stage 6 generates items and puts them into the object pool
class ItemStage < Stage
  def run(data)
    data.consume_each do |elem|
      case elem
      when WeaponPage
        data.push_to_pool Weapon.new(elem.name, elem.type) if elem.type
      end
    end
  end
end

# \Stage 7 produces and places appropriate plants for each food type.
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
      if rand < 0.75 / (current_count + 1)
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

# \Stage 8 makes NPCs from person data and puts them somewhere.
class PersonStage < Stage
  def run(data)
    data.consume_each do |elem|
      case elem
      when PersonPage
        if elem.gender and not elem.occupations.empty?
          npc = NPC.new elem
          data.map.put_somewhere(npc) { |loc| loc.civilized? }
          data.knowledge_base.add_empty(npc).tap do |kn|
            # (1 - Motivation::Damping) here since the primary job will get counted an extra
            # time with damping turned on in the full job list.
            kn.motives += Motivation.motive_for(npc.job) * (1 - Motivation::Damping)
            elem.occupations.each do |n, j|
              kn.motives += Motivation.motive_for(j) * Motivation::Damping
            end
          end
        end
      end
    end
  end
end

# \Stage 9 puts basic fetch quests into the game for each NPC.
class QuestStage < Stage
  def run(data)
    data.knowledge_base.each do |id, val|
      case val
      when NPCBrain
        q = QuestMaker.make_fetch_quest(data.map, val)
        data.add_quests q
        val.add_quest q.id
      end
    end
  end
end

# \Stage 10 finds a starting position for the player and puts him/her there.
class PlayerStage < Stage
  def run(data)
    data.map.put_somewhere Player.new
  end
end

# \Stage 11 makes any final requests necessary for future development of the world
class RequestStage < Stage
  def run(data)
    if data.small_world?
      Request[].tap do |req|
        2.times do
          req << "crawl type: Place base: * count: 1"
        end
        data.push_request req
      end
    end
  end
end
