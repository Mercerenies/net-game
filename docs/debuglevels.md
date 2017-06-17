
# Debug Levels

* 0 - Fatal Errors

  Level 0 is the lowest debug level. Log messages sent at level 0 are *always* printed, so they
  should only be used to print crash information, so that end users can send debugging information
  in the case of a crash.

* 1 - Basic Information / Warnings

  Level 1 is used for the beginnings and endings of each stage of the cycle, as well as major
  process steps within each of the cycles, and important warnings. Only major steps should be
  printed.

* 2 - Detailed Steps

  Level 2 is used for more detailed information and minor steps of processes. When in doubt,
  Level 2 is most frequently the debug level to print to.

* 3 - Verbose Information

  Level 3 is used for miniscule parts of the process. In general, debuggers will use Level 2,
  while Level 3 is used in the case that absolutely detailed information is necessary to solve
  a problem.
