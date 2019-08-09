#!/bin/bash

set -euxo pipefail

GO_PWD="$PWD"

# workaround for https://github.com/ashwanthkumar/gocd-build-github-pull-requests/issues/133
cd grisp-software
GRISP_SOFTWARE_REV=$(git rev-parse HEAD)

rm -rf /opt/grisp
SHORT_SHA1=$(echo "$GRISP_SOFTWARE_REV" | cut -c 1-10)
TOOLCHAIN_BASE_PATH=/opt/grisp/grisp-software/"$1"
TOOLCHAIN_PATH="$TOOLCHAIN_BASE_PATH/$SHORT_SHA1"
mkdir -p "$TOOLCHAIN_BASE_PATH"
ln -s "$GO_PWD"/grisp-software "$TOOLCHAIN_PATH"
cd "$TOOLCHAIN_PATH"
git submodule update --init rtems-source-builder
make all
echo "OK" > rtems-source-builder/rtems/rsb-report-ok

cd /
ARCHIVENAME="$1"_toolchain_arm-rtems5_Linux_$GRISP_SOFTWARE_REV.tar.gz
tar -czf "$GO_PWD"/"$ARCHIVENAME" "$TOOLCHAIN_PATH"/rtems-install
echo "$GRISP_SOFTWARE_REV" > "$GO_PWD"/TOOLCHAIN_REVISION
rm -rf /opt/grisp
