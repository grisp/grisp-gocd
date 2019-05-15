#!/bin/bash

set -euxo pipefail

BUILDDIR=$PWD

set +u; source "$HOME"/.asdf/asdf.sh; set -u

while read -u10 v; do # we read from filenumber ten (stdin is used somwhere within while...)
    cd "$BUILDDIR"
    rm -rf /opt/grisp
    # get rid of rebar3 cache
    rm -rf ~/.cache/rebar3/

    asdf install erlang "$v"
    asdf local erlang "$v"
    asdf local rebar 3.10.0

    # install rebar3_grisp globally
    mkdir -p ~/.config/rebar3
    echo '{plugins, [rebar3_hex, rebar3_grisp]}.' > ~/.config/rebar3/rebar.config

    mkdir -p "$BUILDDIR"/toolchain
    if [[ $GO_PIPELINE_NAME == "grisp-new-toolchain" ]]; then
        # use version from fetched artifact
        cd /
        tar -xzf "$BUILDDIR"/toolchain/grisp_toolchain*.tar.gz
    else
        # fetch master rev from s3
        GRISP_TOOLCHAIN_REVISION=$(git ls-remote -h https://github.com/grisp/grisp-software master | awk '{print $1}')
        cd /
        curl -L https://s3.amazonaws.com/grisp/platforms/grisp_base/toolchain/grisp_toolchain_arm-rtems5_Linux_"${GRISP_TOOLCHAIN_REVISION}".tar.gz | tar -xz
    fi

    ## TODO install custom version of rebar3 plugin. symlink it in ~/.cache/rebar3/plugins

    mkdir "$BUILDDIR"/grisp_release
    cd "$BUILDDIR"
    DEBUG=1 rebar3 new grispapp name=ciproject dest="$BUILDDIR"/grisp_release
    cd "$BUILDDIR"/ciproject

    if [[ $GO_PIPELINE_NAME == "grisp" ]]; then
        # link grisp into _checkouts directory
        mkdir "$BUILDDIR"/ciproject/_checkouts
        ln -s "$BUILDDIR"/grisp "$BUILDDIR"/ciproject/_checkouts/grisp
    fi

    # build otp
    TC_PATH=( /opt/grisp/grisp-software/grisp-base/*/rtems-install/rtems/5 )
    erl -noshell -eval '{ok, Config} = file:consult("rebar.config"),
                        {value, {grisp, GrispConfig}} = lists:keysearch(grisp, 1, Config),
                        NewGrispConfig = GrispConfig ++ [{build, [{toolchain, [{directory, "'${TC_PATH[@]}'"}]}]}],
                        NewGrispConfig2 = lists:keyreplace(otp, 1, NewGrispConfig, {otp, [{version, "'"$v"'"}]}),
                        NewConfig = lists:keyreplace(grisp, 1, Config, {grisp, NewGrispConfig2}),
                        file:write_file("rebar.config", lists:map(fun (E) -> io_lib:format("~p.~n", [E]) end, NewConfig)).' -s init stop

    # build grispapp
    rebar3 grisp build --tar true
    cp _grisp/otp/*/package/grisp_otp_build_*.tar.gz "$BUILDDIR"

    # deploy release
    rebar3 grisp deploy -v 0.1.0 -n ciproject
    cd "$BUILDDIR"/grisp_release
    tar -czf "$BUILDDIR"/grisp_release_"$v".tar.gz .

    rm -rf "$BUILDDIR"/grisp_release "$BUILDDIR"/ciproject
    cd "$BUILDDIR"
done 10< .gocd/erlang_versions
