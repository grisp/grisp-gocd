#!/bin/sh

set -euxo pipefail

 [ -z "$(git -C ../grisp-gocd-config/ status -s)" ] || (echo "grisp-gocd-config is modified manually, aborting"; exit 1)
rm  -vf ../grisp-gocd-config/*.json
python3 pipelines/create_pipelines.py
git -C ../grisp-gocd-config/ add ./*.json
echo "Added changes to grisp-gocd-config repo, please review them manually using git diff --staged, commit and push"
