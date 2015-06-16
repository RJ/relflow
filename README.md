relflow
=======

Relflow is a release workflow utility for erlang projects that use
rebar3. It automates the boring bits of generating a reluppable erlang
release, namely:

* writes .appup files for changed applications
* increments vsn field in .app and .app.src files
* updates rebar.config with new release version (in relx section)

__WARNING__ relflow modifies `.app.src`, `.appup`, and `relx.config` files in-place.
Make sure you have committed all local modifications to git before running relflow.


Use
---

Add the relflow plugin to your `rebar.config`:

<pre>
{plugins, [
    {relflow, ".*", {git, "https://github.com/irccloud/rebar3_relflow.git", {branch, "master"}}}
]}.
</pre>


Then:

    $ rebar3 relflow --help


License
-------

Apache2, same as rebar3.
