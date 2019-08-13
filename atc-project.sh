#!/bin/bash

set -euxo pipefail

BUILDDIR=$PWD

set +u; source "$HOME"/.asdf/asdf.sh; set -u

USE_GRISP_MATERIAL=false
USE_REBAR3_GRISP_MATERIAL=false
USE_GRISP_TOOLS_MATERIAL=false
TOOLCHAIN_FROM_S3=false
BUILD_OTP=true

for i in "$@"; do
    case "$i" in
        --toolchain-from-s3)
            shift
            TOOLCHAIN_FROM_S3=true
            ;;
        --use-grisp-material)
            shift
            USE_GRISP_MATERIAL=true
            ;;
        --erlang-version=*)
            ERLANG_VERSION="${i#*=}"
            shift
            ;;
        --use-rebar3_grisp-material)
            shift
            USE_REBAR3_GRISP_MATERIAL=true
            ;;
        --use-grisp_tools-material)
            shift
            USE_GRISP_TOOLS_MATERIAL=true
            ;;
        --prebuilt-otp)
            shift
            BUILD_OTP=false
            ;;
        *)
            shift
            ;;
    esac
done

cd "$BUILDDIR"
rm -rf /opt/grisp
# get rid of rebar3 cache
rm -rf ~/.cache/rebar3/

asdf install erlang "$ERLANG_VERSION"
asdf local erlang "$ERLANG_VERSION"
asdf local rebar 3.11.1

# install rebar3_grisp globally
mkdir -p ~/.config/rebar3
echo '{plugins, [rebar3_hex, grisp_tools, rebar3_grisp]}.' > ~/.config/rebar3/rebar.config

if [[ "$BUILD_OTP" = true ]]; then
    mkdir -p "$BUILDDIR"/toolchain
    if [[ "$TOOLCHAIN_FROM_S3" = false ]]; then
        # use version from fetched artifact
        cd /
        tar -xzf "$BUILDDIR"/toolchain/grisp_toolchain*.tar.gz
    else
        # fetch master rev from s3
        cd /
        curl -L https://s3.amazonaws.com/grisp/platforms/atc/toolchain/atc_toolchain_arm-rtems5_Linux_latest.tar.gz | tar -xz
    fi
fi

# install custom version of rebar3 plugin. symlink it in ~/.cache/rebar3/plugins
if [[ "$USE_REBAR3_GRISP_MATERIAL" = true ]]; then
    mkdir -p ~/.cache/rebar3/plugins
    ln -s "$BUILDDIR"/rebar3_grisp ~/.cache/rebar3/plugins
fi

if [[ "$USE_GRISP_TOOLS_MATERIAL" = true ]]; then
    mkdir -p ~/.cache/rebar3/plugins
    ln -s "$BUILDDIR"/grisp_tools ~/.cache/rebar3/plugins
fi


mkdir "$BUILDDIR"/grisp_release
cd "$BUILDDIR"/uid

if [[ "$USE_GRISP_MATERIAL" = true ]]; then
    # link grisp into _checkouts directory
    mkdir "$BUILDDIR"/uid/_checkouts
    ln -s "$BUILDDIR"/grisp "$BUILDDIR"/uid/_checkouts/grisp
fi

if [[ "$BUILD_OTP" = true ]]; then
    # build otp
    TC_PATH=( /opt/grisp/grisp-software/atc/*/rtems-4.12 )
    erl -noshell -eval '{ok, Config} = file:consult("rebar.config"),
                        {value, {grisp, GrispConfig}} = lists:keysearch(grisp, 1, Config),
                        NewGrispConfig = GrispConfig ++ [{build, [{toolchain, [{directory, "'${TC_PATH[@]}'"}]}]}],
                        NewGrispConfig2 = lists:keyreplace(otp, 1, NewGrispConfig, {otp, [{version, "'"$ERLANG_VERSION"'"}]}),
                        NewConfig = lists:keyreplace(grisp, 1, Config, {grisp, NewGrispConfig2}),
                        file:write_file("rebar.config", lists:map(fun (E) -> io_lib:format("~p.~n", [E]) end, NewConfig)).' -s init stop

    # build grispapp
    DEBUG=1 rebar3 grisp build --tar true
    cp _grisp/otp/*/package/atc_otp_build_*.tar.gz "$BUILDDIR"
fi
# deploy release
DEBUG=1 rebar3 grisp deploy -v 0.1.0 -n uid
cd "$BUILDDIR"/grisp_release
tar -czf "$BUILDDIR"/atc_release_"$ERLANG_VERSION".tar.gz .

rm -rf "$BUILDDIR"/grisp_release "$BUILDDIR"/uid
cd "$BUILDDIR"
