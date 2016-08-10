#!/usr/bin/ruby

load './ruby/util.rb'
load './ruby/criteria.rb'
load './ruby/numeral.rb'
load './ruby/sampler.rb'

load './ruby/affix.rb'
load './ruby/namer.rb'

load './ruby/level.rb'
load './ruby/node.rb'
load './ruby/map.rb'

load './ruby/feature.rb'
load './ruby/bridge.rb'
load './ruby/building.rb'

load './ruby/structure.rb'
load './ruby/tower.rb'
load './ruby/crater.rb'
load './ruby/forest.rb'
load './ruby/bank.rb'
load './ruby/site.rb'
load './ruby/lake.rb'

load './ruby/objs.rb'
load './ruby/weapon.rb'
load './ruby/person.rb'
load './ruby/food.rb'

load './ruby/spawner.rb'
load './ruby/creatures.rb'

load './ruby/loader.rb'
load './ruby/gdata.rb'
load './ruby/stages.rb'
load './ruby/genner.rb'

require 'json'

data = Loader.load JSON.parse(ARGF.read)
gen = Genner.new data
puts gen.generate.to_sxp
