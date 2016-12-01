#!/usr/bin/ruby

class Page

  def to_json(options = {})
    "{}"
  end

end

class PersonPage < Page
  attr_reader :name, :gender, :occupations

  def initialize(json)
    @name = json['name']
    @gender = json['gender']
    @gender = @gender.intern if @gender
    @occupations = json['occupations'].map { |xx| [xx[1], xx[0].intern] }.to_h
  end

  def to_json(options = {})
    {
      'nature' => 'Person',
      'name' => @name,
      'gender' => @gender,
      'occupations' => @occupations.map { |k, v| [v, k] }
    }.to_json options
  end

end

class PlacePage < Page
  attr_reader :name

  def initialize(json)
    @name = json['name']
    @info = json['info'].map { |xx| [xx[1], xx[0].intern] }.to_h
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

  def to_json(options = {})
    {
      'nature' => 'Place',
      'name' => @name,
      'info' => @info.map { |k, v| [v, k] }
    }.to_json options
  end

end

class WeaponPage < Page
  attr_reader :name

  def initialize(json)
    @name = json['name']
    @info = json['info'].map { |xx| [xx[1], xx[0].intern] }.to_h
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

  def to_json(options = {})
    {
      'nature' => 'Weapon',
      'name' => @name,
      'info' => @info.map { |k, v| [v, k] }
    }.to_json options
  end

end

class MonsterPage < Page
  attr_reader :name, :chaos, :affinity

  def initialize(json)
    @name = json['name']
    @info = json['type']
    @chaos = json['chaos'].intern
    @affinity = json['affinity'].intern
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

end

class AnimalPage < Page
  attr_reader :name, :pack, :speed, :sea, :air, :threat, :size

  def initialize(json)
    @name = json['name']
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
      'name' => @name,
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
  attr_reader :name, :full_name, :plant, :nutrition, :poison

  def initialize(json)
    @name = json['nickname']
    @full_name = json['name']
    @plant = json['plant']
    @plant = @plant.intern if @plant
    @nutrition = json['nutrition']
    @poison = json['poison']
  end

  def to_json(options = {})
    {
      'nature' => 'Food',
      'nickname' => @name,
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
