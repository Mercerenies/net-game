
# Adapted from Rosetta Code ///// (Towers)
module Numeral
  @@symbols = { 1=>'I', 5=>'V', 10=>'X', 50=>'L', 100=>'C', 500=>'D', 1000=>'M' }
  @@subtractors = [ [1000, 100], [500, 100], [100, 10], [50, 10], [10, 1], [5, 1], [1, 0] ]

  def self.to_numeral(num)
    if @@symbols.has_key? num
      @@symbols[num]
    else
      @@subtractors.each.lazy.map do |cut, sub|
        case
        when num > cut
          to_numeral(cut) + to_numeral(num - cut)
        when num >= cut - sub && num < cut
          to_numeral(sub) + to_numeral(num + sub)
        end
      end.find { |x| x }
    end
  end

end
