relflow
=======

Relflow is a release workflow utility for erlang projects that use git and
[rebar3](https://github.com/rebar/rebar3). It automates the boring bits of generating a reluppable erlang
release, namely:

* writes .appup files for changed applications
* increments vsn field in .app.src files
* updates rebar.config with new release version (in relx section)

NB: i've only tested this with standard rebar3 layouts using `apps/` or
`src/` dirs in the project root.

Workflow Example
----------------

You've shipped your first release to production. Probably built using
`rebar3 release` (which relies on `relx` internally).

You tagged your git repo with `first-release` when you shipped the release.

Meanwhile, back in your dev environment, you make a bunch of changes and
fix some bugs. You test your changes, and commit them to git.

Now it's time to ship an update. Here's what you need to
do before building a new release:

* For any apps with added/removed/changed modules, increment the `vsn`
  field in `.app.src`, and write a suitable `.appup` file to migrate upwards.
* Increment the release version in the relx section of `rebar.config`

This is what `relflow` does, so run:

    $ rebar3 relflow -u "first-release"

By default, this will tag and commit the resulting changes to git for
you. To see what it did:

    $ git diff HEAD~1  # view diff for last commit

Now you can build the (upgradable) release for the second version.
Note that the `-u` param here is the erlang release vsn, not a git tag.
(However, it is sensible to tag the release-commit with the same vsn string).

    $ rebar3 release relup -u "first-release"

Rationale & Appup Generation
----------------------------

Existing release upgrade tools require that you have the previous release
sitting on disk where you build the upgrade, and will typically use
[beam_lib:cmp_dirs/2](http://www.erlang.org/doc/man/beam_lib.html#cmp_dirs-2)
to determine which modules have changed, in order to create an appup.

This is a pain, because often releases are build on a CI server remotely.
Relflow depends on git for the list of changed modules. It uses the current
rebar3 profile to examine BEAMs and check if they are gen_servers, supervisors, etc.

The implicit assumption here is that modules don't suddenly change from being supervisors to gen_servers or anything weird. If so, the appup would be invalid and require fixing manually.

For example:

    $ rebar3 as prod relflow -u v12345.6789

This will use `git diff v12345.6789` for a list of changed modules, and then
check _build/prod/$relname/.. for BEAMs in order to create an appup.

See [relflow_appup.erl](https://github.com/RJ/relflow/blob/master/src/relflow_appup.erl) for more details.
In some cases, you may wish to hand-edit appups, if you have specific dependency ordering requirements.


In-place file rewriting
-----------------------

relflow modifies `.app.src` and `rebar.config` files in-place.
Make sure you have committed all local modifications to git before running relflow.

`.app.src` files are `file:consult`ed, then written out
using `io_lib:format` so you lose formatting and comments from them.

`rebar.config` is parsed by line and rewritten to preserve comments
and whitespace, so you need to format the version like this:

    %% somewhere in rebar.config:
    {relx, [
        {release, {myrelname,
        "v1.2.3" %% relflow-release-version-marker
        }, [
            app1, cowboy, etc
        ]},
        {vm_args, "./vm.args"},
        {sys_config, "./sys.config"},
        {extended_start_script, true},
        {include_erts, false}
    ]}.

Specifically, the rebar.config rewriter looks for a line like this:

        "v1.2.3" %% relflow-release-version-marker

..and replaces it like this:

        "new.version.here" %% relflow-release-version-marker

So make sure you format your rebar.config to match.

This is the only line modified in rebar.config.

Use
---

Add the relflow plugin to your `rebar.config`:

<pre>
{plugins, [
    {relflow, ".*", {git, "https://github.com/RJ/relflow.git", {branch, "master"}}}
]}.
</pre>


Help
----

    $ rebar3 help relflow

    Relflow
    =======

    Examples:
        rebar3 relflow -u v1.2.3        # upgrade from last release, at git tag v1.2.3
        rebar3 relflow init-versions    # reset all vsns using relflow format
        rebar3 relflow --version        # print relflow version


    Usage: rebar3 relflow [-u <upfrom>] [-x [<nextver>]] [-g [<autogit>]]
                          [-f [<force>]]

      -u, --upfrom       Git revision/tag to upgrade from (for appup
                         generation)
      -x, --nextversion  The version string to use for the next release
                         [default: auto]
      -g, --autogit      Automatically add and commit relflow changes to git
                         [default: true]
      -f, --force        Force relflow to run even with uncommitted local
                         changes [default: false]
      -v, --version      Print relflow version and exit


License
-------

Apache2, same as rebar3.

Research
--------

Other release / appup generation tools I encountered:

* http://dukesoferl.blogspot.co.uk/2009/05/automatic-appup-file-generation.html (Erlang)
* rebar2 'generate-appups' https://github.com/rebar/rebar/wiki/Upgrades (Erlang)
* https://github.com/boldpoker/edeliver (Erlang)
* https://github.com/davisp/knit (Erlang)
* https://github.com/bitwalker/exrm (Elixir)

Knit has some nifty [module attributes](https://github.com/davisp/knit#knit-module-attributes) to guide the appup creation (module precedence, dependencies, etc). Warrants further study/copying.

Did I miss any? Let me know!

Contact
-------

* RJ2 on freenode
* metabrew on twitter
* rj@metabrew.com
