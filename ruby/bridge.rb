
class Bridge < Feature

  def self.load_bridge(data)
    if data.kind_of? PlacePage
      case data.type
      when :forest
        Forest.new.tap { |o| o.load data }
      when :lake
        Lake.new.tap { |o| o.load data }
      end
    end
  end

  def self.create_random(nodal_size = nil)
    nodal_size ||= (2..6).to_a.sample
    case [1, 2].sample
    when 1
      Forest.new.tap { |o| o.load nil }
    when 2
      Lake.new.tap { |o| o.load nil }
    end
  end

  # Each node in (*nodes) is a list of Node instances as obtained in Node#expand_to_map
  def bridge_on(*nodes)
    nodes.zip(each_exit.cycle) do |curr, exit|
      node = curr.sample
      node.add_link exit.id
      exit.add_link node.id
    end
  end

end

class TrivialBridge < Bridge

  def load(data)
  end

  def bridge_on(*nodes)
    (0 ... nodes.size).each do |i|
      (i + 1 ... nodes.size).each do |j|
        n0 = nodes[i].sample
        n1 = nodes[j].sample
        n0.add_link n1.id
        n1.add_link n0.id
      end
    end
  end

end
