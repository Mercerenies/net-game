
class Titles

  def self.instance
    @@instance ||= Titles.new
  end

  def initialize
    @arr = IO.read('./data/titles.txt').split("\n").map(&:downcase)
  end

  def is_a_title?(str)
    @arr.include? str.downcase
  end

  def self.is_one?(str)
    instance.is_a_title? str
  end

end

class NPC < Person
  attr_accessor :gender, :job, :job_name

  def initialize(data)
    @name = data.name
    @gender = data.gender
    jobs = data.occupations
    @job_name, @job = jobs.to_a.sample
    old_name, old = jobs.to_a.sample
    if old != @job
      @old, @old_name = [old, old_name]
    else
      @old, @old_name = [nil, nil]
    end
  end

  def full_name
    @name
  end

  def name
    name = @name.split ' '
    case name.size
    when 1
      name[0]
    when 2
      if Titles.is_one? name[0]
        name.join ' '
      else
        name[1]
      end
    else
      if Titles.is_one? name[0]
        name[0] + name[-1]
      else
        name[-1]
      end
    end
  end

  def old_job
    @old
  end

  def old_job_name
    @old_name
  end

  def to_sxp
    [:npc, full_name, :':short-name', name, :':gender', @gender, :':job', job, :':job-name', job_name,
     :':old-job', old_job, :':old-job-name', old_job_name].to_sxp
  end

end
