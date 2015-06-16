relflow
=====

release workflow util

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { relflow, ".*", {git, "git@host:user/relflow.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 relflow
    ===> Fetching relflow
    ===> Compiling relflow
    <Plugin Output>
