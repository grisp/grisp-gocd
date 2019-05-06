#!/bin/bash

set -euxo pipefail

# workaround for https://github.com/ashwanthkumar/gocd-build-github-pull-requests/issues/133
GRISP_SOFTWARE_REV=${GO_REVISION_GRISP_SOFTWARE:-$GO_SCM_GRISP_SOFTWARE_LABEL}

cd grisp-software
rm -rf /opt/grisp
SHORT_SHA1=$(echo "$GRISP_SOFTWARE_REV" | cut -c 1-10)
TOOLCHAIN_BASE_PATH="/opt/grisp/grisp-software/grisp-base"
TOOLCHAIN_PATH="$TOOLCHAIN_BASE_PATH/$SHORT_SHA1"
GO_PWD=$PWD
mkdir -p $TOOLCHAIN_BASE_PATH
ln -s $GO_PWD $TOOLCHAIN_PATH
cd $TOOLCHAIN_PATH
./build/build.sh
echo "OK" > rtems-source-builder/rtems/rsb-report-ok
rm -rf /opt/grisp
