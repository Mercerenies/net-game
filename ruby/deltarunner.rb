#!/usr/bin/ruby

load './ruby/everything.rb'

if ARGV.length < 2
  STDERR.puts 'Usage: ./ruby/deltarunner.rb <alpha_load> <delta_save> <everything_save> [everything]...'
  STDERR.puts ' alpha_load is the file to load the old world from.'
  STDERR.puts ' delta_save is the file to save the new delta file to.'
  STDERR.puts ' everything_save is the file to save the excess parse data to.'
  STDERR.puts ' everything are the files to read the online data from; if not supplied, STDIN is used.'
  exit 1
end

debug_level = ARGV.shift.to_i
Logger.instance.debug_level = debug_level

filename = ARGV.shift
deltafile = ARGV.shift
excess = ARGV.shift

data = if ARGV.empty?
         Loader.load JSON.parse(STDIN.read)
       else
         ARGV.collect { |name| Loader.load JSON.parse(IO.read name) }.flatten 1
       end
alpha = GData.from_sxp SXP::Reader::Scheme.read_file filename
gen = DeltaGenner.new data, alpha
gen.generate
Logger.echo 1, "Outputting alpha structure to STDOUT"
puts gen.alpha_structure.to_sxp
File.open(deltafile, 'w') do |file|
  Logger.echo 1, "Outputting delta structure to #{deltafile}"
  file.write gen.delta_structure.to_dsxp
end
File.open(excess, 'w') do |file|
  Logger.echo 1, "Outputting excess to #{excess}"
  file.write gen.data.excess_to_json
end
