#!/bin/bash

set -euxo pipefail

cd ciproject

ARCHIVENAME=$(basename _grisp/otp/*/package/grisp_otp_build_*.tar.gz)
COUNT=$(aws s3 ls s3://grisp/platforms/grisp_base/otp/$ARCHIVENAME | wc -l)
if [[ $COUNT -eq 0 ]]; then
    aws s3 cp otp/*/package/grisp_otp_build_*.tar.gz s3://grisp/platforms/grisp_base/otp/ --acl public-read;
else
    echo File already on S3;
fi 2>&1 > /tmp/deploy_log
