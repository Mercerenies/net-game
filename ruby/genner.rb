
class Genner

  def initialize(everything)
    @arr = everything.clone
    @nodes = []
    @options = {}
    @bridges = []
    @map = nil
    @creatures = CreatureSet.new
    @spawners = SpawnerSet.new
  end

  def has_bridge?
    not @bridges.empty?
  end

  def get_a_bridge
    if has_bridge?
      result = @bridges.sample
      @bridges = @bridges.remove result
      result
    end
  end

  def generate_nodes
    @nodes = []
    @arr = @arr.reject do |elem|
      if elem.kind_of? PlacePage
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

  def generate_bridges
    @bridges = []
    @arr = @arr.reject do |elem|
      if elem.kind_of? PlacePage
        bridge = Bridge.load_bridge elem
        @bridges << bridge if bridge
      end
    end
  end

  def generate_creatures
    # Identify and set up valid creatures
    @arr = @arr.reject { |elem| @creatures.load_from_page elem }
    creatures = @creatures.to_a.shuffle.cycle
    return if @creatures.to_a.empty?
    # Now identify all of the "dangerous" nodes, that is
    # anywhere that should have a creature in it
    needed = @map.select(&:can_have_creatures?).to_a
    # Now put animals / creatures in those spots
    until needed.empty?
      spawner = Spawner.new creatures.next, @map, needed.first, [2, 2, 2, 3].sample
      area = spawner.area_covered.to_a
      needed = needed.reject { |loc| area.include? loc }
      @spawners.push spawner
    end
  end

  def generate_map
    temp = Node.new '', Level.individual
    @nodes.each { |obj| temp << obj }
    @map = Map.new temp.expand_to_map genner: self
  end

  def generate_buildings
    buildings = []
    @arr = @arr.reject do |elem|
      if elem.kind_of? PlacePage
        building = Building.load_building elem
        buildings << building if building
      end
    end
    buildings.each do |building|
      building.each_node { |x| @map.push x }
      building.integrate_with @map
    end
  end

  def generate_items
    @arr = @arr.reject do |elem|
      case elem
      when WeaponPage
        @map.put_somewhere(Weapon.new elem.name, elem.type) if elem.type
      end
    end
  end

  def generate_people
    @arr = @arr.reject do |elem|
      case elem
      when PersonPage
        if elem.gender and not elem.occupations.empty?
          @map.put_somewhere(NPC.new elem) { |loc| loc.civilized? }
        end
      end
    end
  end

  def generate_foods
    foods = CriteriaQueue[]
    @arr = @arr.reject do |elem|
      case elem
      when FoodPage
        foods << Food.new(elem) if elem.plant
      end
    end
    @map.each do |loc|
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

  # TODO Separate all of these complicated stages into some separate objects; this is getting out of hand
  def generate
    # Stage 1 - Generate the main nodal structures
    generate_nodes
    # Stage 2 - Pre-generate any bridges that can be made
    generate_bridges
    # Stage 3 - Convert the nodes into a map
    generate_map
    # Stage 4 - Add buildings to the map
    generate_buildings
    # Stage 5 - Make a list of creatures and put them places
    generate_creatures
    # Stage 6 - Put items into the map
    generate_items
    # Stage 7 - Put plants that grow food on the map
    generate_foods
    # Stage 8 - Make people and put them somewhere
    generate_people
    # Stage 9 - Position the player
    @map.put_somewhere Player.new
    # Return result
    [@map, @creatures, @spawners]
  end

  private :generate_nodes, :generate_node, :generate_map, :generate_buildings,
          :generate_items, :generate_bridges, :generate_creatures,
          :generate_people, :generate_foods

end
