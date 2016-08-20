
require 'forwardable'

class MetaData
  extend Forwardable
  include Enumerable

  def_delegators :@hash, :[], :[]=, :each

  def initialize
    @hash = {}
  end

  def to_h
    @hash.dup
  end

  def to_hash
    to_h
  end

  def to_sxp
    arr = @hash.to_a.flatten 1
    ([:meta] + arr).to_sxp
  end

end
