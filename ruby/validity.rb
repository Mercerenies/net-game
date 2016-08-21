
# Wraps "validity checking" objects such as class names and Plants instances which are used
# for can_have? style methods. This object allows them to be saved/loaded into meta fields in
# the system file.
class ValidityWrapper

  def initialize(inst)
    @inst = inst
  end

  def data
    @inst
  end

  def to_sxp
    case @inst
    when Class
      [:validity, :class, @inst].to_sxp
    when Plants
      [:validity, :plants, @inst.to_ary].to_sxp
    when nil
      [:validity, :none, nil].to_sxp
    else
      super # TODO Throw an error here, probably
    end
  end

  def self.from_sxp(arg)
    type, parm = Reloader.assert_first :validity, arg
    case type
    when :class
      ValidityWrapper.new(Object.const_get parm.to_s)
    when :plants
      ValidityWrapper.new(Plants[*parm.to_a])
    when :none
      ValidityWrapper.new(nil)
    end
  end

end
