#!/usr/bin/ruby

load './ruby/everything.rb'

debug_level = ARGV.shift.to_i # TODO This value is currently unused

data = if ARGV.empty?
         Loader.load JSON.parse(STDIN.read)
       else
         ARGV.collect { |name| Loader.load JSON.parse(IO.read name) }.flatten 1
       end
gen = Genner.new data
puts gen.generate.to_sxp
