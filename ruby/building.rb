
class Building < Feature

  def self.load_building(data)
    if data.kind_of? PlacePage
      case data.type
      when :tower
        Tower.new.tap { |o| o.load data }
      when :crater
        Crater.new.tap { |o| o.load data }
      when :bank
        Bank.new.tap { |o| o.load data }
      when :site
        Site.new.tap { |o| o.load data }
      #when :church
      #  Church.new.tap { |o| o.load data }
      end
    end
  end

  def can_integrate_with?(node)
    true
  end

  def integrate_with(map)
    arr = map.to_ary.select { |x| self.can_integrate_with? x }
    arr = map.to_ary if arr.empty?
    curr = arr.sample
    each_exit do |exit|
      arr0 = curr.each_link.select { |x| self.can_integrate_with? map[x] }
      arr0 = curr.each_link if arr0.to_a.empty?
      curr = map[ arr0.to_a.sample ]
      exit.add_link curr.id
      curr.add_link exit.id
    end
  end

end
