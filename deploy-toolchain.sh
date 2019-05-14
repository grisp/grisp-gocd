#!/bin/bash

set -euxo pipefail

ARCHIVENAME=$(basename toolchain/grisp_toolchain*.tar.gz)
COUNT=$(aws s3 ls s3://grisp/platforms/grisp_base/toolchain/"$ARCHIVENAME" | wc -l)
if [[ $COUNT -eq 0 ]]; then
    aws s3 cp toolchain/"$ARCHIVENAME" s3://grisp/platforms/grisp_base/toolchain/ --acl public-read
else
    echo File already on S3
fi
