#!/usr/bin/ruby

load './ruby/everything.rb'

debug_level = ARGV.shift.to_i
Logger.instance.debug_level = debug_level

data = if ARGV.empty?
         Loader.load JSON.parse(STDIN.read)
       else
         ARGV.collect { |name| Loader.load JSON.parse(IO.read name) }.flatten 1
       end
gen = Genner.new data
gen.generate
Logger.echo 1, "Outputting alpha structure to STDOUT"
puts gen.alpha_structure.to_sxp
