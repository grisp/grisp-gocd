#!/usr/bin/env python3
import os
import jinja2
import json
import itertools

config_repo_dir = "../grisp-gocd-config"

all_otp_versions = [ "20.2", "21.0-rc1", "21.0", "22.0" ]
default_otp_version = [ "21.0" ]
all_material_types = [ "master-material", "fb-material", "pr-material" ]

def product_dict(**kwargs):
    keys = kwargs.keys()
    vals = kwargs.values()
    list_vals = [ i if isinstance(i, (list,)) else [i] for i in vals ]
    for instance in itertools.product(*list_vals):
        yield dict(zip(keys, instance))

def get_grisp_pipelines():
    grisp_matrix_config={
        "grisp": {
            "version": all_otp_versions,
            "grisp_material": all_material_types,
            "rebar3_grisp_material": False,
            "grisp_tools_material": False,
            "fetch_toolchain": False,
            "build_otp" : True,
            "name": lambda otp, grisp, rebar3_grisp, grisp_tools, build : "grisp-"+grisp+"-otp-"+otp,
            "push_otp": True
        },
        "new-toolchain": {
            "version": all_otp_versions,
            "grisp_material": False,
            "rebar3_grisp_material": False,
            "grisp_tools_material": False,
            "fetch_toolchain": True,
            "build_otp" : True,
            "name": lambda otp, grisp, rebar3_grisp, grisp_tools, build : "new-toolchain-otp-"+otp,
            "push_toolchain": True
        },
        "rebar3_grisp": {
            "version": default_otp_version,
            "grisp_material": [],
            "grisp_dep": ["master", "hex"],
            "rebar3_grisp_material": all_material_types,
            "grisp_tools_material": [],
            "grisp_tools_dep": ["master", "hex"],
            "fetch_toolchain": False,
            "build_otp": [False, True],
            "name": lambda otp, grisp, rebar3_grisp, grisp_tools, build :
            "rebar3-grisp"+rebar3_grisp+"-grisp-"+grisp+"-tools-"+grisp_tools+"-build-"+str(build).lower()+"-otp-"+otp
        },
        "grisp_tools": {
            "version": default_otp_version,
            "grisp_material": [],
            "grisp_dep": ["master", "hex"],
            "rebar3_grisp_material": [],
            "rebar3_grisp_dep": ["master", "hex"],
            "grisp_tools_material": all_material_types,
            "fetch_toolchain": False,
            "build_otp": [False, True],
            "name": lambda otp, grisp, rebar3_grisp, grisp_tools, build :
            "grisp-tools-"+rebar3_grisp+"-grisp-"+grisp+"-tools-"+grisp_tools+"-build-"+str(build).lower()+"-otp-"+otp
        }

    }
    grisp_matrix = {}
    for k, v in grisp_matrix_config.items():
        for i in (product_dict(**v)):
            name = i['name'](i['version'],
                             i['grisp_material'],
                             i['rebar3_grisp_material'],
                             i['grisp_tools_material'],
                             i['build_otp'])
            i['pipeline_name'] = name
            grisp_matrix[name] = i

    return grisp_matrix
    #print(str(grisp_matrix))

def render_pipeline(template, pipeline_name, data):
#    print(data)
    template = env.get_template(os.path.join('pipelines', 'templates', template))
    pipeline_json = template.render(**data)
    with open(os.path.join(config_repo_dir, pipeline_name + ".gopipeline.json"), "w") as pipeline:
        pipeline.write(pipeline_json)
    json.loads(pipeline_json) # check if it's valid json

env = jinja2.Environment(loader = jinja2.FileSystemLoader(os.path.abspath('.')))
grisp_matrix = get_grisp_pipelines()
for k, v in grisp_matrix.items():
    print(k,v)
    print('\n')
    #render_pipeline('grisp.j2.gopipeline.json', k, v)
exit()
render_pipeline('deploy-otp-to-s3.j2.gopipeline.json',
                'deploy-otp-to-s3',
                # all otp versions as upstream pipelines
                { 'upstream_pipelines' :
                  [ v['pipeline_name'] for v in grisp_matrix.values() if 'push_otp' in v and v['push_otp'] ]
                }
)

render_pipeline('deploy-toolchain-to-s3.j2.gopipeline.json',
                'deploy-toolchain-to-s3',
                { 'upstream_pipelines' :
                  [ v['pipeline_name'] for v in grisp_matrix.values() if 'push_toolchain' in v and v['push_toolchain'] ]
                }
)
