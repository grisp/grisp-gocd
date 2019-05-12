#!/bin/bash

set -euxo pipefail

GO_PWD=$PWD

# asdf installed
if cd ~/.asdf; then
    git pull origin master
else
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf
fi
git -C ~/.asdf checkout "$(git describe --abbrev=0 --tags)"

set +u
source $HOME/.asdf/asdf.sh
set -u

# Erlang plugin installed
if [ $(asdf plugin-list | grep -c "erlang") -eq 0 ]; then
    asdf plugin-add erlang
fi

# Rebar plugin installed
if [ $(asdf plugin-list | grep -c "rebar") -eq 0 ]; then
    asdf plugin-add rebar https://github.com/Stratus3D/asdf-rebar.git
fi

asdf plugin-update erlang
asdf plugin-update rebar

asdf install erlang 19.0
asdf global erlang 19.0
# build rebar3 with old erlang so it works with any OTP release
asdf install rebar 3.10.0
asdf global rebar 3.10.0
