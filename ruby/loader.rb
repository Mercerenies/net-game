#!/usr/bin/ruby

module TwoColumnData

  def load_two_column(data)
    data.map { |xx| [xx[1], xx[0].intern] }.to_h
  end

  def save_two_column(data)
    data.map { |k, v| [v, k] }
  end

  private :load_two_column, :save_two_column

end

module HasInfoField
  include TwoColumnData

  attr_reader :info

  def initialize_info(data)
    @info = load_two_column data
  end

  def save_info_field
    save_two_column @info
  end

  def type
    key = keyword
    key and @info[key]
  end

  def keyword
    freq = @info.each_with_object(Hash.new(0)) do |(key, val), hash|
      hash[val] += 1
    end
    mode = freq.max_by { |k, v| v }
    mode and @info.detect { |k, v| v == mode[0] }[0]
  end

  private :initialize_info, :save_info_field

end

class Page
  attr_reader :name

  def initialize(name)
    @name = name
  end

  def to_json(options = {})
    "{}"
  end

end

class PersonPage < Page
  include TwoColumnData
  attr_reader :gender, :occupations

  def initialize(json)
    super json['name']
    @gender = json['gender']
    @gender = @gender.intern if @gender
    @occupations = load_two_column json['occupations']
  end

  def to_json(options = {})
    {
      'nature' => 'Person',
      'name' => name,
      'gender' => @gender,
      'occupations' => save_two_column(@occupations)
    }.to_json options
  end

end

class PlacePage < Page
  include HasInfoField
  attr_reader :population

  def initialize(json)
    super json['name']
    initialize_info json['info']
    @population = json['population']
  end

  def to_json(options = {})
    {
      'nature' => 'Place',
      'name' => name,
      'info' => save_info_field,
      'population' => population
    }.to_json options
  end

  def population_count
    @population or 0
  end

  def population_size
    Population.size @population
  end

end

class WeaponPage < Page
  include HasInfoField

  def initialize(json)
    super json['name']
    initialize_info json['info']
  end

  def to_json(options = {})
    {
      'nature' => 'Weapon',
      'name' => name,
      'info' => save_info_field
    }.to_json options
  end

end

class MonsterPage < Page
  include HasInfoField

  attr_reader :chaos, :affinity

  def initialize(json)
    super json['name']
    initialize_info json['type']
    @chaos = json['chaos'].intern
    @affinity = json['affinity'].intern
  end

  def to_json(options = {})
    {
      'nature' => 'Monster',
      'name' => name,
      'type' => save_info_field,
      'chaos' => @chaos,
      'affinity' => @affinity
    }.to_json options
  end

end

class AnimalPage < Page
  attr_reader :pack, :speed, :sea, :air, :threat, :size

  def initialize(json)
    super json['name']
    @pack = json['pack']
    @speed = json['speed']
    @sea = json['sea']
    @air = json['air']
    @threat = json['threat']
    @size = json['size']
    @matches = json['matches'] # TODO Reject on low match count
  end

  def to_json(options = {})
    {
      'nature' => 'Animal',
      'name' => name,
      'pack' => @pack,
      'speed' => @speed,
      'sea' => @sea,
      'air' => @air,
      'threat' => @threat,
      'size' => @size,
      'matches' => @matches
    }.to_json options
  end

end

class FoodPage < Page
  attr_reader :full_name, :plant, :nutrition, :poison

  def initialize(json)
    super json['nickname']
    @full_name = json['name']
    @plant = json['plant']
    @plant = @plant.intern if @plant
    @nutrition = json['nutrition']
    @poison = json['poison']
  end

  def to_json(options = {})
    {
      'nature' => 'Food',
      'nickname' => name,
      'name' => @full_name,
      'plant' => @plant,
      'nutrition' => @nutrition,
      'poison' => @poison
    }.to_json options
  end

end

module Loader

  def self.whitelist
    [PersonPage, PlacePage, WeaponPage, AnimalPage, FoodPage, MonsterPage]
  end

  def self.load(json)
    arr = json.collect do |expr|
      begin
        name = Object.const_get "#{expr['nature']}Page"
        name.new expr if whitelist.include? name
      rescue NameError
        nil
      end
    end
    arr.reject &:nil?
  end

end
