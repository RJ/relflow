relflow
=======

Relflow is a release workflow utility for erlang projects that use
[rebar3](https://github.com/rebar/rebar3). It automates the boring bits of generating a reluppable erlang
release, namely:

* writes .appup files for changed applications
* increments vsn field in .app and .app.src files
* updates rebar.config with new release version (in relx section)


In-place file rewriting
-----------------------

relflow modifies `.app.src`, `.appup`, and `rebar.config` files in-place.
Make sure you have committed all local modifications to git before running relflow.

`.appup` and `.app.src` files are `file:consult`ed, then written out
using `io_lib:format` so you lose formatting and comments from them.

`rebar.config` is carefully parsed and rewritten to preserve comments
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
    {relflow, ".*", {git, "https://github.com/irccloud/rebar3_relflow.git", {branch, "master"}}}
]}.
</pre>


Then:

    $ rebar3 help relflow


License
-------

Apache2, same as rebar3.
