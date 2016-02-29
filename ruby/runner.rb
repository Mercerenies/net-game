#!/usr/bin/ruby

load './ruby/numeral.rb'
load './ruby/sampler.rb'

load './ruby/affix.rb'
load './ruby/namer.rb'

load './ruby/level.rb'
load './ruby/node.rb'
load './ruby/map.rb'

load './ruby/building.rb'
load './ruby/objs.rb'

load './ruby/loader.rb'
load './ruby/genner.rb'

require 'json'

data = Loader.load JSON.parse(ARGF.read)
gen = Genner.new data
puts gen.generate.to_sxp
