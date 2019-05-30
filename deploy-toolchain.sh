#!/bin/bash

set -euxo pipefail

ARCHIVENAME=$(basename toolchain/grisp_toolchain*.tar.gz)
set +o pipefail
COUNT=$(aws s3 ls s3://grisp/platforms/grisp_base/toolchain/"$ARCHIVENAME" | wc -l)
set -o pipefail

if [[ $(cat toolchain/branch) = "master" ]]; then
    aws s3 cp toolchain/"$ARCHIVENAME" s3://grisp/platforms/grisp_base/toolchain/grisp_toolchain_arm-rtems5_Linux_master.tar.gz --acl public-read
fi

if [[ $COUNT -eq 0 ]]; then
    aws s3 cp toolchain/"$ARCHIVENAME" s3://grisp/platforms/grisp_base/toolchain/ --acl public-read
else
    ®echo File already on S3
fi > deploy_log 2>&1
