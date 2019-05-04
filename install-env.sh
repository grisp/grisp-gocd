#!/bin/bash

set -euxo pipefail

# asdf installed
if cd ~/.asdf; then
    git pull origin master
else
    git clone https://github.com/asdf-vm/asdf.git ~/.asdf
fi
git -C ~/.asdf checkout "$(git describe --abbrev=0 --tags)"

$HOME/.asdf/asdf.sh

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

asdf install erlang $1

asdf install erlang 17.5.6.9
asdf global erlang 17.5.6.9
# build rebar3 with old erlang so it works with any OTP release
asdf install rebar $2

asdf global erlang $1
asdf global rebar $1
