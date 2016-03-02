
require 'forwardable'
require 'sxp'

class Map
  extend Forwardable
  include Enumerable

  def_delegators :@ary, :each, :push

  def initialize(ary = [])
    @ary = ary
  end

  def to_ary
    @ary
  end

  def to_a
    to_ary
  end

  def to_sxp
    ([:map] + @ary).to_sxp
  end

  def [](val)
    @ary.find { |x| x.id == val }
  end

  def put_somewhere(obj)
    weight = Proc.new { |x| 1 / (x.count_items + 1) }
    total = @ary.map( &weight ).reduce(:+)
    num = rand total
    result = @ary.detect do |x|
      num -= weight.(x)
      num <= 0
    end
    if result
      result.push obj
      true
    else
      false
    end
  end

end

class Location
  extend Forwardable
  include Enumerable

  attr_reader :id, :name, :country_name

  def_delegators :@contents, :each, :[], :[]=, :push, :delete, :pop
  def_delegator :@links, :each, :each_link

  def initialize(id, name, country_name)
    @id = id
    @name = name
    @country_name = country_name
    @contents = []
    @links = []
  end

  def long_name
    if @country_name
      "#{@name}, #{@country_name}"
    else
      @name
    end
  end

  def add_link(x)
    @links.push x unless @links.include? x
  end

  def to_sxp
    prefix = [:location, @id, @name]
    country = @country_name ? [:':country', @country_name] : []
    links = [:':links', @links.dup]
    contents = [:':contents', @contents.dup]
    (prefix + country + links + contents).to_sxp
  end

  def count_items
    self.count { |x| x.kind_of? Item }
  end

end
