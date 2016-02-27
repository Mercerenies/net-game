#!/usr/bin/ruby

load './ruby/sampler.rb'

load './ruby/affix.rb'
load './ruby/namer.rb'

load './ruby/level.rb'
load './ruby/node.rb'
load './ruby/map.rb'

load './ruby/loader.rb'
load './ruby/genner.rb'

require 'json'

data = Loader.load JSON.parse(ARGF.read)
gen = Genner.new data
gen.generate
puts gen.instance_variable_get(:@map).to_sxp
