
# Wraps "validity checking" objects derived from +Validator+. This object allows them to be saved/loaded
# into meta fields in the system file.
#
# <b>DEPRECATED</b>: Now that using class names as validators is no longer supported, there is no
# advantage to using +ValidityWrapper+ over simply serializing +Validator+ instances directly. As
# such, this interface may be removed in the future.
class ValidityWrapper

  def initialize(inst)
    warn "[DEPRECATION] Wrapping in `ValidityWrapper` instances should no longer be necessary"
    @inst = inst
  end

  def data
    @inst
  end

  def to_sxp
    case @inst
    when PlantTypesValidator
      [:validity, :plants, @inst.to_ary].to_sxp
    when LandBasedValidator
      [:validity, :'land-only', nil].to_sxp
    when AnimalValidator
      [:validity, :'any-animal', nil].to_sxp
    when EmptyValidator
      [:validity, :none, nil].to_sxp
    else
      raise "Unknown validator #{@inst}"
    end
  end

  def self.from_sxp(arg)
    type, parm = Reloader.assert_first :validity, arg
    case type
    when :plants
      ValidityWrapper.new(PlantTypesValidator[*parm.to_a])
    when :'land-only'
      ValidityWrapper.new(LandBasedValidator.new)
    when :'any-animal'
      ValidityWrapper.new(AnimalValidator.new)
    when :none
      ValidityWrapper.new(EmptyValidator.new)
    end
  end

end

class Validator

  def ===(obj)
    warn "Warning: Using old Validator interface"
    include? obj
  end

  def include?(obj)
    false
  end

end

# TODO Sea creatures can still chase the player off the lake areas; they need to be incapable of doing so
class LandBasedValidator < Validator

  def include?(obj)
    obj.kind_of? Animal and not obj.sea?
  end

  def to_sxp
    [:'land-only'].to_sxp
  end

  def self.from_sxp(arg)
    Reloader.assert_first :'land-only', arg
    LandBasedValidator.new
  end

end

class AnimalValidator < Validator

  def include?(obj)
    obj.kind_of? Animal
  end

  def to_sxp
    [:'any-animal'].to_sxp
  end

  def self.from_sxp(arg)
    Reloader.assert_first :'any-animal', arg
    AnimalValidator.new
  end

end

class PlantTypesValidator < Validator

  def initialize(ary)
    @ary = ary
  end

  def self.[](*args)
    PlantTypesValidator.new args
  end

  def to_ary
    @ary
  end

  def to_a
    to_ary
  end

  def include?(obj)
    obj.respond_to? :plant_type and @ary.include? obj.plant_type
  end

  def to_sxp
    [:plants, to_ary].to_sxp
  end

  def self.from_sxp(arg)
    ary = Reloader.assert_first :plants, arg
    PlantTypesValidator.new ary.dup
  end

end

class EmptyValidator < Validator

  def include?(obj)
    false
  end

  def to_sxp
    [:'no-validator'].to_sxp
  end

  def self.from_sxp(arg)
    Reloader.assert_first :'no-validator', arg
    EmptyValidator.new
  end

end
