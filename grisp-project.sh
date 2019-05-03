#!/bin/bash

BUILDDIR=$PWD
$HOME/.asdf/asdf.sh

# install rebar3_grisp
if [[$1 = "hex"]]; then
    echo '{plugins, [rebar3_hex, rebar3_grisp]}.' >> ~/.config/rebar3/rebar.config
else
    echo '{plugins, [rebar3_hex, {rebar3_grisp, {git, "https://github.com/grisp/rebar3_grisp.git",
                        {ref, "'$1'"}}}]}.' > ~/.config/rebar3/rebar.config
fi


# install toolchain
if [[ $2 == "HEAD" ]]; then
    GRISP_TOOLCHAIN_REVISION=$(git ls-remote -h https://github.com/grisp/grisp-software master | awk '{print $1}')
else
    GRISP_TOOLCHAIN_REVISION=$2
fi
cd /tmp
wget https://s3.amazonaws.com/grisp/platforms/grisp_base/toolchain/grisp_toolchain_arm-rtems5_Linux_${GRISP_TOOLCHAIN_REVISION}.tar.gz
cd /opt && tar -xzf /tmp/grisp_toolchain_arm-rtems5_Linux_${GRISP_TOOLCHAIN_REVISION}.tar.gz
rm /tmp/grisp_toolchain*

# create grisp project
rebar3 new grispapp ciproject dest=/tmp/ciproject

# set grisp version
# in case we build grisp repo link it to _checkouts
# otherwise use hex version
if [[ $3 == "true" ]]; then
    mkdir $BUILDDIR/_checkouts
    ln -s $BUILDDIR/ciproject/_checkouts/grisp $BUILDDIR/grisp
fi

cd $BUILDDIR/ciproject

if [[ $4 == "true" ]]; then
    TC_PATH=( /opt/grisp/grisp-software/grisp-base/*/rtems-install/rtems/5 )
    erl -noshell -eval '{ok, Config} = file:consult("rebar.config"),
                        {value, {grisp, GrispConfig}} = lists:keysearch(grisp, 1, Config),
                        NewGrispConfig = GrispConfig ++ [{build, [{toolchain, [{directory, "'${TC_PATH[@]}'"}]}]}],
                        NewConfig = lists:keyreplace(grisp, 1, Config, {grisp, NewGrispConfig}),
                        file:write_file("rebar.config", lists:map(fun (E) -> io_lib:format("~p.~n", [E]) end, NewConfig)).' -s init stop

    # build grispapp
    rebar3 grisp build --tar true
fi

rebar3 grisp deploy -v 0.1.0 -n ciproject
