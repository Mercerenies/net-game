
module Util
  def self.titlecase(x)
      x.split(' ').map(&:capitalize).join(' ')
  end
end
