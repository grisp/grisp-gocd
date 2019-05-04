#!/bin/bash

set -euxo pipefail

cd grisp-software

GRISP_TOOLCHAIN_REVISION=$(cat rtems-install/rtems/5/GRISP_TOOLCHAIN_REVISION)
ARCHIVENAME=grisp_toolchain_arm-rtems5_Linux_$GRISP_TOOLCHAIN_REVISION.tar.gz
COUNT=$(aws s3 ls s3://grisp/platforms/grisp_base/toolchain/$ARCHIVENAME | wc -l)
if [[ $COUNT -eq 0 ]]; then
    tar -czf /tmp/$ARCHIVENAME .
    aws s3 cp /tmp/grisp_toolchain_*.tar.gz s3://grisp/platforms/grisp_base/toolchain/ --acl public-read
    rm /tmp/grisp_toolchain*
else
    echo File already on S3
fi 2>&1 > /tmp/deploy_log
