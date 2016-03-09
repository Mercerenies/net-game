
module Util

  def self.titlecase(x)
      x.split(' ').map(&:capitalize).join(' ')
  end

  # http://stackoverflow.com/questions/14859120/calculating-median-in-ruby
  def self.median(array)
    sorted = array.sort
    len = sorted.length
    (sorted[(len - 1) / 2] + sorted[len / 2]) / 2.0
  end

end
