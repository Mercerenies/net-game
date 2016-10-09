
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
    if data
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
    else
      @name = ''
      @gender = nil
      @job_name, @job, @old, @old_name = [nil, nil, nil, nil]
    end
    @quest_list = []
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

  def add_quest(qid)
    # The quest list is a list of quest identifiers
    @quest_list.push qid
  end

  def quest_count
    @quest_list.size
  end

  def to_sxp
    [:npc, full_name, :':short-name', name, :':gender', @gender, :':job', job, :':job-name', job_name,
     :':old-job', old_job, :':old-job-name', old_job_name, :':quest-list', @quest_list].to_sxp
  end

  def self.from_sxp(arg)
    name, *arr = Reloader.assert_first :npc, arg
    ReloadedNPC.new(name).tap do |npc|
      Reloader.hash_like(arr) do |k, v|
        case k
        when :':short-name'
          # Ignore this arg; it can (usually) be calculated from name
        when :':gender'
          npc.gender = v
        when :':job'
          npc.job = v
        when :':job-name'
          npc.job_name = v
        when :':old-job'
          npc.old_job = v
        when :':old-job-name'
          npc.old_job_name = v
        when :':quest-list'
          v.each { |q| npc.add_quest q }
        end
      end
    end
  end

end

class ReloadedNPC < NPC
  attr_writer :job, :job_name

  def initialize(name)
    super(nil)
    @name = name
  end

  def old_job=(val)
    @old = val
  end

  def old_job_name=(val)
    @old_name = val
  end

end
