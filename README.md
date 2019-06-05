Build Scripts for Grisp GOCD Pipelines and Pipeline Templates
=============================================================

Pipelines
---------

### Requirements

- python3
- python3-jinja2

### Changing the Pipelines

Have a look in `pipelines/create_pipelines.py` and the templates in `pipelines/templates/`.

### Workflow

This script assumes you have [grisp-gocd-config](https://github.com/grisp/grisp-gocd-config) checked out in `../grisp-gocd-config/`. When you execute `make` in this directory `pipelines/create_pipelines` will delete the contents of `../grisp-gocd-config/`, create new pipelines based on the `pipelnes/create_pipelines.py` and the templates in `pipelines/templates/`. Please verify the staged changes manually in the `grisp-gocd-config` repository, commit and push them. The [GoCD instance](https://public.ci.stritzinger.com) will shortly pick up the changes in the configrepo.

To clean the `grisp-gocd-config` repo run `make clean`.

Build Scripts
-------------

- `install-env.sh`: Installs rebar3, and Erlang
- `grisp-project.sh`: Build OTP release with all possible Erlang versions:
    - `--use-grisp-material`: Will link the grisp material into the `_checkouts` directory
    - `--erlang-version 21.0`: Only test with OTP 21.0
    - `--use-rebar3-grisp-material`: Will globally install the grisp plugin provided by a material
- `deploy-toolchain.sh`: Deploys the toolchain to S3
- `deploy-otp.sh`: Deploys OTP to S3
