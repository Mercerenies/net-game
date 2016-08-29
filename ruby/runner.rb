#!/usr/bin/ruby

load './ruby/everything.rb'

data = Loader.load JSON.parse(ARGF.read)
gen = Genner.new data
puts gen.generate.to_sxp

# This is temporary to test things ; it WILL NOT be in the final version in this form

#gen.generate
#puts DeltaGData.new(gen.data, []).result_structure.to_sxp

#sxp = SXP::Reader::Scheme.read_file "./temp/system1.txt"
#gdata = GData.from_sxp sxp
#STDERR.puts gdata.result_structure.to_sxp
