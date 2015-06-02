# Relflow - Erlang release workflow helper

A tool to do all the boring bits of generating a reluppable erlang
release.

Automatically:

* writes .appup files for changed applications
* increments vsn field in .app and .app.src files
* updates relx.config with new release version

It's intended to work alongside `rebar3` release building (which
uses `relx`).

__WARNING__ relflow modifies `.app.src`,
`.appup`, and `relx.config` files in-place.
Make sure you have committed all local modifications to git before running relflow.

## Installation

* Have [rebar3](http://www.rebar3.org/) installed.
* `rebar3 escriptize`
* `sudo cp _build/default/bin/relflow /usr/bin/relflow`

## Workflow Example

Where rel-xxx-previous is the git tag of the release you want to upgrade
from:

    $ relflow -u rel-xxx-previous

Relflow will now:

* Check the git diff (ie: `git diff relx-xxx-previous`)
* Generate an .appup for any application with changed modules (per the diff)
* Rewrite .app.src files to increment the app vsn, for changed applications
* Rewrite the rebar.config or relx.config to increment the release version

At this point, rebar has rewritten/created some files for you.
Review what changed with git diff.

Now you're ready to:

    $ rebar3 release relup tar  (ie, run relx)

and at no point did you have to mess around incrementing versions of
apps and releases or write the appup files (just review the auto-genned
ones, and possibly edit if needed).

### How relflow rewrites files

**appup** files are rewritten using io_lib:format, which removes comments
and formatting.

The rebar/relx config is modified to preserve formatting,
so you need to isolate and comment the version line like this:

    {release, {foo,
    "version.goes.here" %% relflow-release-version-marker
    }, ...

so that relflow can rewrite the file while preserving whitespace and
comments.

