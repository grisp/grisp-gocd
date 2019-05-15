#!/bin/bash

set -euxo pipefail

for otp in otp/grisp_otp_build_*.tar.gz; do
    ARCHIVENAME=$(basename $otp)
    COUNT=$(aws s3 ls s3://grisp/platforms/grisp_base/otp/"$ARCHIVENAME" | wc -l)
    if [[ $COUNT -eq 0 ]]; then
        aws s3 cp otp/"$ARCHIVENAME" s3://grisp/platforms/grisp_base/otp/ --acl public-read
    else
        echo File already on S3;
    fi >> deploy_log 2>&1
done
