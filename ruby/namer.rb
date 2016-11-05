
# An object which manages the generation of natural-sounding names of a specific type. The default Namer
# instance is designed to construct city and country names, but other instances are available.
class Namer

  # Returns the default city/country name generator.
  def self.instance
    @@instance ||= Namer.new
  end

  # Constructs a name generator. The generator uses a Markov chain algorithm of order +order+ (default +2+),
  # using a special termination algorithm. The +termination+ algorithm determines how quickly the algorithm will
  # attempt to terminate. Higher termination values will result in shorter generated names.
  def initialize(fname: "./data/naming.txt", order: 2, termination: 0.05)
    @order = order
    @term = termination
    @hash = {}
    File.open fname do |file|
      file.each do |name|
        name = name.chomp + ' '
        (- order .. name.length - order - 1).each do |i|
          if i < 0
            subseq = name[0, order + i + 1]
          else
            subseq = name[i, order + 1]
          end
          @hash[ subseq[0 .. -2] ] ||= Hash.new 0
          @hash[ subseq[0 .. -2] ][ subseq[-1] ] += 1
        end
      end
    end
  end

  def inspect
    to_s
  end

  # Generates a name using the name generator's rules and data.
  def sample
    curr = ""
    multiplier = 0
    loop do
      distr = @hash[ curr[- @order, @order ] || curr ]
      break unless distr
      total = distr.values.reduce(0, &:+)
      break if rand(total) < multiplier * distr[' ']
      rnd = rand total
      curr += distr.detect { |k, v| (rnd -= v) <= 0 }[0]
      multiplier += @term
    end
    curr = curr.strip
    curr = curr[0 .. -3] if curr =~ / [^ ]$/ # Remove single-letter word endings
    curr
  end

end

# A module containing a natural landform Namer instance.
module Natural
  # Returns the natural Namer instance for landforms, constructing it on-demand if it does not exist.
  def self.namer
    @@namer ||= Namer.new(fname: './data/naming_natural.txt')
  end
end
