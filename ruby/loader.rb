#!/usr/bin/ruby

class Person
  attr_reader :name, :gender, :occupations

  def initialize(json)
    @name = json['name']
    @gender = json['gender'].intern
    @occupations = json['occupations'].to_h
  end

end

class Place
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

class Weapon
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

module Loader

  def self.whitelist
    [Person, Place, Weapon]
  end

  def self.load(json)
    arr = json.collect do |expr|
      begin
        name = Object.const_get expr['nature']
        name.new expr if whitelist.include? name
      rescue NameError
        nil
      end
    end
    arr.reject &:nil?
  end

end
