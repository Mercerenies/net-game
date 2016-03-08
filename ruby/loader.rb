#!/usr/bin/ruby

class PersonPage
  attr_reader :name, :gender, :occupations

  def initialize(json)
    @name = json['name']
    @gender = json['gender'].intern
    @occupations = json['occupations'].to_h
  end

end

class PlacePage
  attr_reader :name, :type, :keyword

  def initialize(json)
    @name = json['name']
    if json['info']
      @type = json['info'][0].intern
      @keyword = json['info'][1]
    else
      @type = nil
      @keyword = nil
    end
  end

end

class WeaponPage
  attr_reader :name, :type, :keyword

  def initialize(json)
    @name = json['name']
    if json['info']
      @type = json['info'][0].intern
      @keyword = json['info'][1]
    else
      @type = nil
      @keyword = nil
    end
  end

end

class AnimalPage
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

end

module Loader

  def self.whitelist
    [PersonPage, PlacePage, WeaponPage, AnimalPage]
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
