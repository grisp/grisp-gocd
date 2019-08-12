#!/bin/bash

set -euxo pipefail

ARCHIVENAME=$(basename toolchain/"$1"-toolchain*.tar.gz)
set +o pipefail
COUNT=$(aws s3 ls s3://grisp/platforms/"$1"/toolchain/"$ARCHIVENAME" | wc -l)
set -o pipefail


if [[ $COUNT -eq 0 ]]; then
    aws s3 cp toolchain/"$ARCHIVENAME" s3://grisp/platforms/"$1"/toolchain/ --acl public-read
    aws s3 cp toolchain/"$ARCHIVENAME" s3://grisp/platforms/"$1"/toolchain/"$1"-toolchain_arm-rtems5_Linux_latest.tar.gz --acl public-read
else
    echo File already on S3
fi > deploy_log 2>&1
