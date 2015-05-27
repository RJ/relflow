# Relflow - Erlang release workflow helper

__This is currently being refactored to use git to track diffs between
releases - feel free to ask me about this on IRC, RJ2 on freenode__

Automatically..

* writes .appup files
* increments vsn field in .app and .app.src files
* updates relx.config with new release info

It's intended to work alongside ````rebar3```` with `relx`
release-building..

__WARNING__ relflow modifies ````.app````, ````.app.src````,
````.appup````, and ````relx.config```` files in-place.
Make sure you have committed all local modifications to git before running relflow.


## Workflow Example

Where rel-xxx-previous is the git tag of the release you want to upgrade
from:

    $ relflow -u rel-xxx-previous

This will figure out which app versions to increment, do so by modifying
the relevant .app and .app.src files in-place, write out appropriate
.appup files for changed applications, and then add a new release
section to your relx.config file with a new version string.

You can run ````relflow```` without any arguments, and it will guess all
the appropriate configuration options.

At this point, review what changed with git diff.

Now you're ready to:

    $ rebar3 release ...  (ie, run relx)

and at no point did you have to mess around incrementing versions of
apps and releases or write the appup files (just review the auto-genned
ones, and possibly edit if needed).

Relflow guesses whether to do a minor or patch bump of version numbers,
depending on severity of modifications. TODO: make this configurable.

Next step is to turn that into debs using the as-yet unreleased new
tool, or just append "tar" to the relx command to deploy with tarball.

## relflow --help

Relflow will guess everything, unless you specify with a command-line
option.

| Short | Long         | Type    | Description                                            |
|:-----:|:------------:|:-------:|--------------------------------------------------------|
| -n    | --relname    | string  | Release name, guessed from relx.config                 |
| -p    | --relpath    | string  | Release dir, defaults to ./_rel/$relname               |
| -u    | --upfrom     | string  | Previous release version, guessed from RELEASES file   |
| -r    | --relxfile   | string  | Path of relx.config, defaults to ./relx.config         |
| -l    | --loglevel   | string  | Log level: debug,info,warn,error(default: info)        |
| -v    | --version    |         | Print relflow version                                  |
| -h    | --help       |         | Print usage message                                    |


## Build and install

Create a standalone executable escript and copy into your PATH:

    $ make install

