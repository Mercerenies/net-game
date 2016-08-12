net-game
========

An adaptive game that uses current world data to produce an in-game world for the player to explore.

The reader, which scans the Internet for current information, is written in Python. Then, a Perl script parses through the data that was found and collects important information by searching for keywords and patterns. Next, Ruby takes the pattern matched data and generates a game world, which it passes onto a Common Lisp frontend to interact with the user.

There are a few shell scripts that are provided for convenience. The only one most users need concern themselves with is `./bash/master.sh`. Run this file from the top directory (not from within `./bash/`) to load data and/or play the game. It will call all of the other necessary scripts as needed, provided the path is set up correctly and all interpreters exist.

Commands in the game are entered at the command line. Use `help` in-game to get the list of commands. Note that the `quit` command will always exit the game, regardless of the current game mode.

This game (specifically, the Python part) accesses the Internet. Your antivirus software may not like that. This game ONLY accesses Wikipedia and pages on the Wikipedia domain; feel free to check the Python code to verify that this is the case. It also depends on the Wikipedia package for Python, which is available online and through `pip` for free and is usable under the MIT license.

# Getting Started

Once you've cloned this repository, you'll want to run `./bash/check.sh` to determine any dependencies you're missing. Note that all scripts should be run with the top-level project directory as the current directory. The script will not attempt to install any missing dependencies but will simply report them; see the Dependencies section below for details. Once the check script passes, you're ready to play. You can use `./bash/master.sh --help` to get a full set of commands, but a good initial gameplay session can be obtained through the following command:

    $ ./bash/master.sh -P 3 -w 4 -a 4 -1 -2 -3 -4 -l <name-of-common-lisp-implementation>

The `-P`, `-w`, and `-a` arguments tell the system to find places, weapons, and animals (the numbers correspond to the number of attempts). The `-1 -2 -3 -4` specifies that all four stages of the pipeline should be run, and the `-l ...` specifies the name of your Common Lisp implementation (since many such implementations install themselves in non-standard locations).

Note that, if you lose the game and would like to try again with the same world data, simply leave off the `-1 -2 -3` and the system will only run the Common Lisp portion of the pipeline. Likewise, if you want to keep the pages but randomly generate a new world with the same information, use `-3 -4` to only run the latter two stages of the pipeline.

For full details on the master script, run `./bash/master.sh --help` or see the corresponding section below.

# Dependencies

Note that Python, Perl, and Ruby are expected to be in /usr/bin. The Lisp implementation must be on the path. Windows users are strongly urged to run this game in Cygwin or some other Unix-like shell. The game is being developed using Cygwin on Windows 7, so I can vouch for the fact that it should work like that. Note that the script `./bash/check.sh` will check for the necessary languages and modules and report what is missing.

###### Python
* Python 3
* Wikipedia module (`pip install wikipedia`)

###### Perl
* Perl 5.10 or newer
* JSON::PP (usually comes with Perl implementations)
* XML::Simple (some people have a problem loading this; make sure libxml is on your path, especially on Windows)

###### Ruby
* Ruby 2.1 or newer
* SXP gem (`gem install sxp`)

###### Common Lisp
* Any conforming Common Lisp implementation (with CLOS)
(NOTE: The system defaults to assuming GNU CLISP is on the path. If you wish to use another implementation, you may have to pass its name as a command line argument to some scripts.)

# Master Script Arguments

There are several scripts in the `./bash/` directory, but (aside from the initial `./bash/check.sh` script) the entrypoint to all of these is `./bash/master.sh`. Running the script with `--help` as the only option will print a summary of the command line flags available.

 * `-c` Celebrities
 * `-p` People
 * `-P` Places
 * `-w` Weapons
 * `-m` Monsters
 * `-a` Animals
 * `-f` Foods
The first several flags control the number of crawls to perform for each type of entity. Note that this is the number of *attempts*, not necessarily the number of results. If you would like five locations, it is often a good idea to run `-P 6` or `-P 7` to be safe, since some of the crawls are likely to fail.

 * `-d` Debug Level
The debug level controls the information printed to STDERR. An absent (or 0) debug level results in nothing being printed to STDERR. At debug level 1, the Python engine will print all of the pages it visits during its crawl. At level 2, the reinforcement engine will print whether or not it is in "exploration mode".

 * `-r` Reinforcement Engine
The reinforcement learning engine is an experimental learning machine encoded into the Wikipedia crawling algorithm. As it is highly experimental, it is disabled by default but can be enabled with this flag. The engine will remember past crawls and use them to improve its ability to search through the Internet. Note that, even with this flag, some types of crawls will not use the reinforcement engine anyway, as it is not necessary in all cases.

 * `-1` Site Crawling
 * `-2` Page Parsing
 * `-3` World Generation
 * `-4` Gameplay
These flags control which stages of the game to run. The first stage crawls Wikipedia to obtain pages. The second stage scans these pages and outputs certain information that it was able to scrape from them. The third stage generates a game world. The fourth stage is the only interactive stage, which allows you to play the game itself. In general, the first and second stages should be run as a set, getting new game information and scanning through it. The third stage will generate a new world from the current information, and the fourth stage will simply start a new game in the current world.

 * `-l` Lisp Implementation
This flag allows you to customize your Common Lisp implementation. If you use a Lisp implementation other than GNU CLISP, you must supply its name and any command line arguments it requires using this flag.
