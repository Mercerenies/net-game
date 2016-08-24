
require 'sxp'

class ListLikeChain

  def initialize(head, *rest)
    @arr = [head] + rest
  end

  def header
    sxp = @arr.first.to_sxp
    # TODO Really? Do we have to convert then read back to get this? Do better.
    expr = SXP::Reader::Scheme.read sxp
    expr.first
  end

  def to_sxp
    # Figure out the header, then throw the rest at the end
    ([header] + to_a).to_sxp
  end

  def to_a
    @arr.map(&:to_a).flatten(1).to_a
  end

  def to_ary
    to_a
  end

end
