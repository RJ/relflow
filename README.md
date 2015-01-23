# Relflow - Erlang release workflow helper

Automatically..

* writes .appup files
* increments vsn field in .app and .app.src files
* updates relx.config with new release info

It's intended to work alongside ````relx````, since it expects to find
your previous release under ````./_rel/$relname````

## Workflow Example

Your last deployed release was versioned "1.0.0", and as such you have a
````./_rel/$relname/releases/1.0.0```` dir, and associated
````./_rel/$relname/lib/...```` dirs.

You make some changes to various ````.erl```` files, run ````rebar
compile````, test, and are ready to create+deploy the release.

    $ relflow -u 1.0.0 -n $relname

This will figure out which app versions to increment, do so by modifying
the relevant .app and .app.src files in-place, write out appropriate
.appup files for changed applications, and then add a new release
section to your relx.config file with a new version string.

At this point, review what changed with git diff.

Now you're ready to:

    $ relx -u 1.0.0 -v 1.1.0 -n $relname release relup

and at no point did you have to mess around incrementing versions of
apps and releases or write the appup files (just review the auto-genned
ones, and possibly edit if needed).

Next step is to turn that into debs using the as-yet unreleased new
tool, or just append "tar" to the relx command to deploy with tarball.

## relflow --help

Relflow will guess everything, unless you specific with a command-line
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

    $ make
    $ sudo cp ./relflow /usr/bin/

