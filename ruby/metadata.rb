
require 'forwardable'

class MetaData
  extend Forwardable
  include Enumerable

  def_delegators :@hash, :[], :[]=, :each

  def initialize(arg = nil)
    @hash = arg.to_h
  end

  def to_h
    @hash.dup
  end

  def to_hash
    to_h
  end

  def to_s
    "#<MetaData:0x#{object_id.to_s 16} @hash=#{@hash}>"
  end

  def to_sxp
    arr = @hash.to_a.flatten 1
    ([:meta] + arr).to_sxp
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :meta, arg
    MetaData.new Hash[*arr]
  end

end
