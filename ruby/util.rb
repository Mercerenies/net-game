
module Util

  # Given a string, capitalizes the first letter of each word.
  def self.titlecase(x)
      x.split(' ').map(&:capitalize).join(' ')
  end

  # Finds the median of the array given.
  def self.median(array)
    # From http://stackoverflow.com/questions/14859120/calculating-median-in-ruby
    sorted = array.sort
    len = sorted.length
    (sorted[(len - 1) / 2] + sorted[len / 2]) / 2.0
  end

end
