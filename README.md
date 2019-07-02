GRiSP-GOCD
==========

Repository for the GoCD CI system we use for GRiSP development.

Buildscripts
-------------

Those scripts are added as a material to GoCD pipelines and they help building and testing the GRiSP components.

Configrepo grisp-gocd-config/
------------------------------

The [configrepo](https://github.com/grisp/grisp-gocd-config) should be checked out inside `grisp-gocd-condig`. It is gitignored. The pipeline definitions stored in JSON files reside there.

generate_pipelines
--------------------

The pipelines are created using an escript.

###Build

    $ rebar3 escriptize

###Run

    $ _build/default/bin/generate_pipelines

Todo
-----

- Add Makefile to automaticly checkout and deal with the grisp-gocd-config repo
- Automaticly generate the configrepo in a pipeline
- Add Mix pipelines
