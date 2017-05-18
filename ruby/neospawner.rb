
# This will eventually become the standard spawner and is merely called "neo-spawner" for
# backward compatibility during the migration

class NeoSpawner
  attr_accessor :creature_id

  def initialize(creature)
    @time = nil
    @creature_id = creature.id
  end

  def time
    @time
  end

  def time=(val)
    # Time should be either nil or an integer
    @time = val && val.to_i
  end

  def to_sxp
    prefix = [:'neo-spawner']
    creature = [creature_id]
    counter = []
    counter = [:':time', time] if time
    (prefix + creature + counter).to_sxp
  end

  def self.from_sxp(arg)
    creature, *arr = Reloader.assert_first :'neo-spawner', arg
    NeoSpawner.new(creature).tap do |spawn|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':time'
          spawn.time = v
        end
      end
    end
  end

end
