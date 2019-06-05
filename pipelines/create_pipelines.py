#!/usr/bin/env python3

import os
import jinja2
import json
import itertools

config_repo_dir = "../grisp-gocd-config"

all_otp_versions = [ "20.2", "21.0-rc1", "21.0" ]
default_otp_version = [ "21.0" ]

grisp_matrix={
    "grisp": {
        "versions": all_otp_versions,
        "grisp_material": True,
        "rebar3_grisp_material": False,
        "grisp_tools_material": False,
        "fetch_toolchain": False,
        "build_otp" : True
    },
    "new-toolchain": {
        "versions": all_otp_versions,
        "grisp_material": False,
        "rebar3_grisp_material": False,
        "grisp_tools_material": False,
        "fetch_toolchain": True,
        "build_otp" : True
    }
}


def create_rebar3_conf(grisp, tools, build):
    return { "rebar3_grisp_"+grisp[0]+"_tools_"+tools[0]+"_build_"+str(build[0]) : {
        "versions": default_otp_version,
        "grisp_material": grisp[1],
        "rebar3_grisp_material": True,
        "grisp_tools_material": tools[1],
        "fetch_toolchain": False,
        "build_otp": build[1]
    }}

# (PIPELINE_NAME, MATERIAL_VALUE)
grisp_combinations = [ ("master", True), ("hex", False) ]
tools_combinations = [ ("master", True), ("hex", False) ]
build_combinations = [ ("true", True), ("false", False) ]

rebar3_combinations = list(itertools.product(grisp_combinations, tools_combinations, build_combinations))

for combination in rebar3_combinations:
    conf = create_rebar3_conf(*combination)
    for name, parameters in conf.items():
        grisp_matrix[name] = parameters

env = jinja2.Environment(loader = jinja2.FileSystemLoader(os.path.abspath('.')))

def create_pipeline(template, pipeline_name, data):
    print(data)
    template = env.get_template(os.path.join('pipelines', 'templates', template))
    pipeline_json = template.render(**data)
    with open(os.path.join(config_repo_dir, pipeline_name + ".gopipeline.json"), "w") as pipeline:
        pipeline.write(pipeline_json)
    json.loads(pipeline_json) # check if it's valid json

def get_grisp_pipeline_name(pipeline_type, otp_version):
    return "grisp-"+pipeline_type+"-otp-"+otp_version

# grisp pipelines:
for pipeline_type, item in grisp_matrix.items():
    for v in item["versions"]:
        pipeline_name = get_grisp_pipeline_name(pipeline_type, v)
        create_pipeline('grisp.j2.gopipeline.json',
                        pipeline_name,
                        {
                            "pipeline_name" : pipeline_name,
                            "erlang_version" : v,
                            "grisp_material" : item["grisp_material"],
                            "rebar3_grisp_material" : item["rebar3_grisp_material"],
                            "grisp_tools_material" : item["grisp_tools_material"],
                            "fetch_toolchain" : item["fetch_toolchain"],
                            "build_otp" : item["build_otp"]
                        }
        )
create_pipeline('deploy-otp-to-s3.j2.gopipeline.json',
                'deploy-otp-to-s3',
                # all otp versions as upstream pipelines
                { 'upstream_pipelines' :
                  [ get_grisp_pipeline_name('grisp', v) for v in grisp_matrix["grisp"]["versions"] ]
                }
)

create_pipeline('deploy-toolchain-to-s3.j2.gopipeline.json',
                'deploy-toolchain-to-s3',
                { 'upstream_pipelines' :
                  [ get_grisp_pipeline_name('new-toolchain', v) for v in grisp_matrix['new-toolchain']['versions'] ]
                }
)
