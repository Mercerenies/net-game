net-game
========

An adaptive game that uses current world data to produce an in-game world for the player to explore.

The reader, which scans the Internet for current information, is written in Python. Then, a Perl script parses through the data that was found and collects important information by searching for keywords and patterns. Next, Ruby takes the pattern matched data and generates a game world, which it passes onto a Common Lisp frontend to interact with the user.

There are a few shell scripts that are provided for convenience. The only one most users need concern themselves with is `./bash/master.sh`. Run this file from the top directory (not from within `./bash/`) to load data and/or play the game. It will call all of the other necessary scripts as needed, provided the path is set up correctly and all interpreters exist.

Commands in the game are entered at the command line. Use `help` in-game to get the list of commands. Note that the `quit` command will always exit the game, regardless of the current game mode.

This game (specifically, the Python part) accesses the Internet. Your antivirus software may not like that. This game ONLY accesses Wikipedia and pages on the Wikipedia domain; feel free to check the Python code to verify that this is the case. It also depends on the Wikipedia package for Python, which is available online and through `pip` for free and is usable under the MIT license.

# Getting Started

Once you've cloned this repository, you'll want to run `./bash/check.sh` to determine any dependencies you're missing. Note that all scripts should be run with the top-level project directory as the current directory. The script will not attempt to install any missing dependencies but will simply report them; see the Dependencies section below for details. Once the check script passes, you're ready to play.

There are currently two different ways to play the game: Master Mode and Client Mode. Client Mode requires additional dependencies such as the Lua programming language but has the added benefit of being self-modifying. Master Mode is still fully supported through `./bash/master.sh`. The following command is a good initial play.

    $ ./bash/master.sh -e 'basic-crawl' -1 -2 -3 -4

The `-1 -2 -3 -4` specifies that all four stages of the pipeline should be run. The `-e` portion supplies an expression directing the crawling engine. The `basic-crawl` command is a simple built-in command designed to make it easy for new users to get started playing. When you're moderately comfortable with the system, you may consider looking at the documentation for `crawl`, the full-fledged crawling command, which has more capabilities but is also more unwieldy.

Note that, if you lose the game and would like to try again with the same world data, simply leave off the `-1 -2 -3` and the system will only run the Common Lisp portion of the pipeline. Likewise, if you want to keep the pages but randomly generate a new world with the same information, use `-3 -4` to only run the latter two stages of the pipeline.

For full details on the master script, run `./bash/master.sh --help` or see the corresponding section below.

# Dependencies

Note that Python, Perl, and Ruby are expected to be in /usr/bin. The Lisp implementation must be on the path. This program is designed to be run in a Unix environment. I believe Cygwin should suffice for Windows users, but I cannot guarantee complete compatibility. Note that the script `./bash/check.sh` will check for the necessary languages and modules and report what is missing.

###### Python
* Python 3
* Wikipedia module (`pip3 install wikipedia`)
* BeautifulSoup 4 module (`pip3 install bs4`, should install automatically as a dependency of the Wikipedia module)

###### Perl
* Perl 5.16 or newer
* JSON::PP (usually comes with Perl implementations)
* XML::Simple (some people have a problem loading this; make sure libxml is on your path, especially on Windows)

###### Ruby
* Ruby 2.1 or newer
* SXP gem (`gem install sxp`)

###### Common Lisp
* GNU CLISP (if you wish to use another implementation, you merely need to write a small shim inside of `./lisp/os.lisp` to bridge the gap)

###### Lua
* Lua 5.2 or newer
* LuaSocket module

###### Bash
* Bash (any reasonably modern version should suffice)
* `flock` (only required for the reinforcement learning engine)

###### Emacs
* Emacs 25 or newer (only necessary if you wish to use the Emacs integrated debugger)

# Client Mode

Client Mode allows the game to communicate with its own internal server and modify itself to make gameplay last longer. Client Mode requires a recent Lua implementation, as well as GNU CLISP (other CLISP implementations may work with some amount of effort; see above).

To run the game in Client Mode, use `./bash/client.sh` (no arguments). The game will start almost immediately, but the world will be relatively empty. After a short period of time wandering around, you will start to notice more people and objects in the game world.

# Emacs Integration

The `./emacs/client.el` file provides some very primitive and very experimental support for integrating with Emacs. In order to use it, you need Emacs 25 or newer. Simply load the file and then use `M-x net-game-run` from the base directory of the project. See the comments at the top of the `./emacs/client.el` file for details.

# Master Mode

There are several scripts in the `./bash/` directory, but (aside from the initial `./bash/check.sh` script) the entrypoint to all of these is `./bash/master.sh`. Running the script with `--help` as the only option will print a summary of the command line flags available.

 * `-d` Debug Level

   The debug level controls the information printed to STDERR. An absent (or 0) debug level results in nothing being printed to STDERR. At debug level 1, basic information will be printed about processes. At debug level 2, more detailed useful information will be printed as things happen. At debug level 3, almost everything that occurs will be reported verbosely. In general, debug level 2 suffices for most tasks.

 * `-e` Crawl Expression

   The expression provided here should be a sequence of commands, delimited by semicolons, which will direct the Web crawling engine in its path. For full documentation on the expression syntax, see `./docs/commands.md`.

 * `-1` Site Crawling
 * `-2` Page Parsing
 * `-3` World Generation
 * `-4` Gameplay

   These flags control which stages of the game to run. The first stage crawls Wikipedia to obtain pages. The second stage scans these pages and outputs certain information that it was able to scrape from them. The third stage generates a game world. The fourth stage is the only interactive stage, which allows you to play the game itself. In general, the first and second stages should be run as a set, getting new game information and scanning through it. The third stage will generate a new world from the current information, and the fourth stage will simply start a new game in the current world.

 * `-l` Lisp Implementation

   This flag allows you to customize your Common Lisp implementation. If you use a Lisp implementation other than GNU CLISP, you must supply its name and any command line arguments it requires using this flag.
