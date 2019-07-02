-module(generate_pipelines_deploy_toolchain).

-export([get_template_data/1]).

get_template_data(GrispData) ->
    [{upstream_pipeline,
     lists:foldl(fun
                     (E, UpstreamPipelines) ->
                         case lists:keysearch(push_toolchain, 1, E) of
                             false -> UpstreamPipelines;
                             {value, {push_toolchain, false}} -> UpstreamPipelines;
                             {value, {push_toolchain, true}} ->
                                 {value, {name, Name}} = lists:keysearch(name, 1, E),
                                 [[{name, Name}]|UpstreamPipelines]
                        end
                 end, [], GrispData)
    }].
