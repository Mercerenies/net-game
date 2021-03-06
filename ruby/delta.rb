
# This is a tag module for any delta object. That is, every object that is designed to model changes
# to existing world data should include this module. Non-abstract implementors of +Delta+ should have
# a 0-ary #to_dsxp method which returns a #to_sxp-ready list.
module Delta
  # Implementors of Delta are expected to have a to_dsxp 0-ary method which returns a to_sxp-ready list.
end

# This is a tag module for objects which have a +Delta+ equivalent. Non-abstract implementors of +DeltaAble+
# should have a 0-ary #to_delta method which returns a corresponding +Delta+ object.
module DeltaAble
  # Implementors of DeltaAble are expected to have a to_delta 0-ary method which returns a Delta object.
end
