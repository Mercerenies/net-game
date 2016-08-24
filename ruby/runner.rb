#!/usr/bin/ruby

load './ruby/util.rb'
load './ruby/criteria.rb'
load './ruby/numeral.rb'
load './ruby/sampler.rb'
load './ruby/listlike.rb'

load './ruby/affix.rb'
load './ruby/namer.rb'

load './ruby/level.rb'
load './ruby/node.rb'
load './ruby/map.rb'
load './ruby/deltamap.rb'

load './ruby/quest.rb'

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
load './ruby/validity.rb'

load './ruby/loader.rb'
load './ruby/reloader.rb'
load './ruby/metadata.rb'
load './ruby/gdata.rb'
load './ruby/deltagdata.rb'
load './ruby/stages.rb'
load './ruby/genner.rb'

require 'json'
require 'sxp'

data = Loader.load JSON.parse(ARGF.read)
gen = Genner.new data
puts gen.generate.to_sxp

# This is temporary to test things ; it WILL NOT be in the final version in this form

#gen.generate
#puts DeltaGData.new(gen.data, []).result_structure.to_sxp

#sxp = SXP::Reader::Scheme.read_file "./temp/system1.txt"
#gdata = GData.from_sxp sxp
#STDERR.puts gdata.result_structure.to_sxp
