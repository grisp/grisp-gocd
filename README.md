Build Scripts for Grisp GOCD Pipelines
======================================

Templates:
-----------

### grisp-software

Expected Materials:

* grisp-gocd in `.gocd/`
* grisp-software in `grisp-software/`

Artifacts:

* `grisp_toolchain*.tar.gz`
* `grisp-software/rtems-source-builder/rtems/rsb-report*`

Single stage/job/task: `build-toolchain.sh`

### grisp

Expected Materials:

* Pipeline: `grisp-software/`
* grisp-gocd in `.gocd`
* grisp in `grisp/`
* rebar3_grisp in `rebar3_grisp`

Artifacts:

* OTP
* GRiSP-Release

If material `grisp-software` has changed pull Artifact via URL, else get latest master from S3.

`install-env.sh` all required Erlang versions

`grisp-project.sh`: Build OTP release with all possible Erlang versions:

* set `asdf local erlang`
* `rebar3 grisp build`
* mv OTP tar to buildroot
* `rebar3 grisp deploy`
* archive grispproject to buildroot
* `rm -r _build _grisp`

| Changed        | toolchain | grisp | rebar3-grisp |
|----------------|-----------|-------|--------------|
| grisp-software | HEAD      | hex   | hex          |
| grisp          | master    | HEAD  | hex          |
| rebar3_grisp   | master    | hex   | HEAD         |


grisp-project.sh
-----------------

Options:

- `--use-grisp-material`: Will link the grisp material into the `_checkouts` directory
- `--erlang-version 21.0`: Only test with OTP 21.0
- `--use-rebar3-grisp-material`: Will globally install the grisp plugin provided by a material
