#!/usr/bin/ruby

load './ruby/everything.rb'

if ARGV.length < 2
  STDERR.puts 'Usage: ./ruby/deltarunner.rb <alpha_load> <delta_save>'
  STDERR.puts ' alpha_load is the file to load the old world from.'
  STDERR.puts ' delta_save is the file to save the new delta file to.'
  exit 1
end

filename = ARGV[0]
deltafile = ARGV[1]

data = Loader.load JSON.parse(STDIN.read)
alpha = GData.from_sxp SXP::Reader::Scheme.read_file filename
gen = DeltaGenner.new data, alpha
puts gen.generate.to_sxp
File.open(deltafile, 'w') do |file|
  file.write gen.delta_structure.to_dsxp
end
