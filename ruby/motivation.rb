
module Motivation

  @@motivations = %i{knowledge wealth glory comfort conquest protection compassion}
  @@jobs_table = nil

  Damping = 0.33 # Damping factor for secondary jobs

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

  def self.is_motive?(x)
    @@motivations.include? x
  end

  def self.motive_for(job)
    jobs_table[job]
  end

end

class MotivePriorities

  def initialize(hash)
    @hash = hash.to_h
  end

  def initialize_copy(orig)
    @hash = orig.to_h.dup
  end

  def to_h
    @hash
  end

  def to_hash
    @hash
  end

  def [](motive)
    @hash[motive]
  end

  def []=(motive, value)
    @hash[motive] = value
  end

  def /(value)
    @hash = @hash.map { |k, x| [k, x / value] }
  end

  def *(value)
    @hash = @hash.map { |k, x| [k, x / value] }
  end

  def +(value)
    @hash.merge!(value.to_h) { |k, o, n| o + n }
  end

  def to_sxp
    [:motives, to_h.flatten].to_sxp
  end

  def self.from_sxp(arg)
    hash, *ignore = Reloader.assert_first :motives, arg
    MotivePriorities.new hash.each_slice(2).to_h
  end

end
