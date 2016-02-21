#!/usr/bin/ruby

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

  def initialize(fname: "./data/naming.txt", order: 2)
    @order = order
    @hash = {}
    File.open fname do |file|
      file.each do |name|
        name = name.chomp + ' '
        (- order .. name.length - order - 2).each do |i|
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
    len = 4 + rand(10)
    len.times do |i|
      distr = @hash[ curr[- @order, @order ] || curr ]
      break unless distr
      rnd = rand (distr.values.reduce(0, &:+))
      curr += distr.detect { |k, v| (rnd -= v) <= 0 }[0]
    end
    curr
  end

end
