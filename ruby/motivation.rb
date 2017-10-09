
# NPCs have motivations which result in quests for the player. Depending on an NPCs occupation and
# other factors, the NPC values different motivations differently, having a different set of needs.
# This module encapsulates the different motivations for different occupations.
module Motivation

  @@motivations = %i{knowledge wealth glory comfort conquest protection compassion}
  @@jobs_table = nil

  Damping = 0.25 # Damping factor for secondary jobs

  # Returns the hash table mapping jobs to MotivePriorities instances, loading it from the
  # file if it has not yet been loaded.
  def self.jobs_table
    if @@jobs_table.nil?
      @@jobs_table = {}
      File.open("./data/motives.txt", 'r') do |file|
        lines = file.each_line
        title, *header = lines.next.split(/  +/)
        header = header.map { |x| x.downcase.intern }
        lines.each do |line|
          name, *figures = line.split(/  +/)
          @@jobs_table[name.intern] = MotivePriorities.new header.zip(figures.map &:to_i)
        end
      end
    end
    @@jobs_table
  end

  # Returns whether the symbol given denotes a valid NPC motivation.
  def self.is_motive?(x)
    @@motivations.include? x
  end

  # Returns the MotivePriorities instance for the specified job, or a zero priority instance
  # if the job is not recognized.
  def self.motive_for(job)
    if jobs_table.include? job
      jobs_table[job]
    else
      MotivePriorities.new
    end
  end

end

# A MotivePriorities instance represents a mapping from a given motive to a number representing
# how much the given entity values the motive. The numbers are designed to be used as relative
# values, to be compared to one another, and should not be treated as absolute quantities.
class MotivePriorities

  # Constructs a priority listing from the given hash.
  def initialize(hash)
    @hash = hash.to_h
  end

  # When a MotivePriorities instance is copied, the underlying hash is copied too.
  def initialize_copy(orig)
    @hash = orig.to_h.dup
  end

  # Returns the hash representing this data.
  def to_h
    @hash
  end

  # Returns the hash representing this data.
  def to_hash
    @hash
  end

  # Returns the value of the given motivation, or zero.
  def [](motive)
    @hash[motive] ||= 0
  end

  # Assigns a value to the given motivation.
  def []=(motive, value)
    @hash[motive] = value
  end

  # Divides each quantity in this priority listing by the specified numerical value.
  def /(value)
    MotivePriorities.new @hash.map { |k, x| [k, x / value] }
  end

  # Multiplies each quantity in this priority listing by the specified numerical value.
  def *(value)
    MotivePriorities.new @hash.map { |k, x| [k, x * value] }
  end

  # Combines the two motive instances together, adding up any matching terms in each.
  def +(value)
    MotivePriorities.new @hash.merge(value.to_h) { |k, o, n| o + n }
  end

  # Combines the two motive instances together, subtracting any matching terms in each.
  def -(value)
    MotivePriorities.new @hash.merge(value.to_h) { |k, o, n| o - n }
  end

  def to_sxp
    [:motives, to_h.flatten].to_sxp
  end

  def self.from_sxp(arg)
    hash, *ignore = Reloader.assert_first :motives, arg
    MotivePriorities.new hash.each_slice(2).to_h
  end

end
