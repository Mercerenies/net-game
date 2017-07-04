
# Command Syntax

The Web crawling engine supports a special command syntax for directing the way it travels the Web. Here, you will find details on the different commands which are supported. Commands are supplied to the engine by passing `-e` to `master.sh` or to `stage1.sh`. Do not pass `-e` multiple times; if you need multiple commands, separate them with a semicolon or a newline in the expression string.

Individual commands consist of a command name followed by any number of named arguments. Each named argument consists of the name of the argument (which always ends in a colon to denote it as a named argument key) followed by its value. Currently, there are four types of arguments.

* Strings - Any token enclosed in \[square brackets\] is considered to be a string. If square brackets or backslashes need to be included in the string, they must be escaped with a backslash.
* Integers - More specifically, natural numbers. Any token which consists only of digits is considered to be an integer.
* Boolean - The literal tokens "yes" and "no" (case insensitive) are considered to be Booleans corresponding to true and false, respectively.
* Symbols - Any token which is none of the above types is a symbol. Symbols are case insensitive, unlike strings, and are used to denote command names, argument keys, and enumeration values.

## Commands

The following built-in commands are available.

### basic-crawl

`basic-crawl` takes no arguments. It performs two standard crawls for people, three for places, and three for animals, with suitable default values passed in. This command is largely intended for new users who do not yet know how to use the command system.

#### Examples

    basic-crawl

### legacy-crawl

`legacy-crawl` is supported for backward compatibility with the old argument-passing syntax for the crawling engine. It has one required argument and performs the appropriate crawls.

* `args:` (string; required)

  The value of the `args:` argument should be a string suitable for passing to the standard `getopt` command line utility. Its syntax is documented fully in the `legacy_stage1.sh` command, which takes the same basic argument set plus `-d` and uses `legacy-crawl` in its implementation.

#### Examples

    legacy-crawl args: [-p 2]
    legacy-crawl args: [-P 3 -a 1 -r]
    legacy-crawl args: [-p 2 -P 3 -a 3] # This command is equivalent to basic-crawl

### crawl

`crawl` is the most powerful crawling command and also the most general. It takes several arguments, only a few of which are explicitly required.

* `type:` (symbol; required)

  The type of page to search for, as one of `celeb`, `person`, `place`, `weapon`, `monster`, `animal`, or `food`. If `type:` is `*` then it will take its value from `base:`, which must then be a symbol. It is an error for both `type:` and `base:` to be `*`.

* `base:` (symbol or string; required)

  The starting page. If this is a string, the Wikipedia page with the given name will be used as the starting page. If this is a symbol, it will use the "standard" starting page for the corresponding type of page, which should be a valid type according to `type:`. If `base:` is `*`, it will take its value from `type:`. It is an error for `type:` and `base:` to both be `*`, and it is an error for `type:` to be `*` while `base:` is a string.

* `depth:` (integer; defaults to 5)

  When performing a crawl, this value specifies how many pages deep to go before giving up and starting over.

* `tries:` (integer: defaults to 3)

  After the depth has been exhausted and the engine determines that it is necessary to start over, this is the number of times that it will do so before giving up entirely and reporting that no match was found.

* `count:` (integer; defaults to 1)

  The engine will attempt to find this many matching pages. Note that it may find fewer, if one or more of the crawls fails completely.

* `rein:` (Boolean; defaults to no)

  If this value is true, the reinforcement learning engine will be enabled, which theoretically produces better results as time goes on, as it will remember which branches are more likely to be fruitful.

#### Examples

    crawl type: Person base: * count: 2
    crawl type: Place depth: 2 tries: 4 base: [List of mountains in the United States]
    crawl base: Celeb type: * rein: yes
