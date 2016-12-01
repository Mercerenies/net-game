
# A Logger class which conditionally outputs debug information to STDERR, depending on the debug level set
# when the program was invoked.
class Logger

  # Returns the default logger instance. Note that Logger is not a singleton class; new instances can
  # be freely created; this is merely a default instance provided for programmer convenience.
  def self.instance
    @@instance ||= Logger.new
  end

  # Echo the string to the default logger instance, using #echo.
  def self.echo(level, str)
    Logger.instance.echo level, str
  end

  def initialize(debug_level = 0)
    @level = debug_level
  end

  def debug_level
    @level
  end

  def debug_level=(val)
    @level = val.to_i
  end

  # Outputs the information given in the string argument if (and only if) the current debug level for this
  # logger is greater than or equal to the supplied level.
  def echo(level, str)
    STDERR.puts "#{$$} [3] #{str}" if debug_level >= level
  end

end
