net-game
========

An adaptive game that uses current world data to produce an in-game world for the player to explore.

The reader, which scans the Internet for current information, is written in Python. Then, a Perl script parses through the data that was found and collects important information by searching for keywords and patterns. Next, Ruby takes the pattern matched data and generates a game world, which it passes onto a Common Lisp frontend to interact with the user.

There are a few shell scripts that are provided for convenience. The only one most users need concern themselves with is `./bash/master.sh`. Run this file from the top directory (not from within `./bash/`) to load data and/or play the game. It will call all of the other necessary scripts as needed, provided the path is set up correctly and all interpreters exist.

Commands in the game are entered at the command line. Use `help` in-game to get the list of commands. Note that the `quit` command will always exit the game, regardless of the current game mode.

This game (specifically, the Python part) accesses the Internet. Your antivirus software may not like that. This game ONLY accesses Wikipedia and pages on the Wikipedia domain; feel free to check the Python code to verify that this is the case. It also depends on the Wikipedia package for Python, which is available online and through `pip` for free and is usable under the MIT license.

#Dependencies

Note that Python, Perl, and Ruby are expected to be in /usr/bin. The Lisp implementation must be on the path. Windows users are strongly urged to run this game in Cygwin or some other Unix-like shell. The game is being developed using Cygwin on Windows 7, so I can vouch for the fact that it should work like that. Note that the script `./bash/check.sh` will check for the necessary languages and modules and report what is missing.

######Python
* Python 3
* Wikipedia module (`pip install wikipedia`)

######Perl
* Perl 5 (tested using Perl 5.10)
* JSON::PP (usually comes with Perl implementations)
* XML::Simple (some people have a problem loading this; make sure libxml is on your path, especially on Windows)

######Ruby
* Ruby 2.1 or newer
* SXP gem (`gem install sxp`)

######Common Lisp
* Any conforming Common Lisp implementation (with CLOS)
(NOTE: The system defaults to assuming GNU CLISP is on the path. If you wish to use another implementation, you may have to pass its name as a command line argument to some scripts.)
