
class Building < Feature

  def self.load_building(data)
    if data.kind_of? PlacePage
      case data.type
      when :tower
        Tower.new.tap { |o| o.load data }
      when :crater
        Crater.new.tap { |o| o.load data }
      #when :church
      #  Church.new.tap { |o| o.load data }
      end
    end
  end

  def integrate_with(map)
    curr = map.to_ary.sample
    each_exit do |exit|
      curr = map[ curr.each_link.to_a.sample ]
      exit.add_link curr.id
      curr.add_link exit.id
    end
  end

end
