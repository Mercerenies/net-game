
# A Request object will be SXP-ed and passed onto the Lisp system, which will make a request
# to the Python system on its behalf.
class Request
  attr_reader :expr

  # Constructs a raw request object. No processing is done on the expression, which should be
  # a string. That is, if this constructor is called directly, the expression should already
  # have its strings escaped, and the caller is responsible for the correctness of the text.
  def initialize(expr)
    @expr = expr
  end

  # Converts the Request object to an S-expression.
  def to_sxp
    # The middle slot is always +nil+ and is reserved for a possible ID value in the future.
    [:request, nil, expr].to_sxp
  end

  def self.from_sxp(arg)
    ignored, expr = Reloader.assert_first :request, arg
    Request.new expr
  end

  # Escapes a string in preparation for encoding in an expression. All open and close square
  # brackets, as well as any backslashes, are preceded with a backslash.
  def self.escape(str)
    # Precede each instance of [, ], or \ with a backslash.
    str.gsub /([\[\\\]])/, '\\\1'
  end

  # Stringifies a string, which escapes it and then wraps it in square brackets.
  def self.stringify(str)
    "[#{self.escape str}]"
  end

end

class RequestSet
  include Enumerable

  # TODO We would like to take all of the ???Set classes and factor them into one common type.

  def initialize
    @reqs = []
  end

  def push(x)
    case x
    when Request
      @reqs << x
      true
    end
    false
  end

  def each(&block)
    each_request &block
  end

  def each_request(&block)
    @reqs.each &block
  end

  def to_sxp
    ([:'request-set'] + to_a).to_sxp
  end

  def self.from_sxp(arg)
    arr = Reloader.assert_first :'request-set', arg
    RequestSet.new.tap do |set|
      Reloader.list_like(arr) { |x| set.push x }
    end
  end

  def empty?
    @reqs.empty?
  end

end
