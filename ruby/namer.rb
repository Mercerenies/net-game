
=begin
require 'singleton'

class Ratio
  attr_accessor :numer, :denom

  def initialize(n, m)
    @numer = n
    @denom = m
  end

  def to_r
    Rational(@numer, @denom)
  end

  def rationalize(eps = 0.0)
    to_r
  end

end
=end

class Namer

  # Not a singleton; I'm just providing a utility instance
  def self.instance
    @@instance ||= Namer.new
  end

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

module Natural
  def self.namer
    @@namer ||= Namer.new(fname: './data/naming_natural.txt')
  end
end
