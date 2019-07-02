-module(generate_pipelines_deploy_otp).

-export([get_template_data/1]).

get_template_data(GrispData) ->
    [{upstream_pipeline,
     lists:foldl(fun
                     (E, UpstreamPipelines) ->
                         case {lists:keysearch(push_otp, 1, E), lists:keysearch(gitmaterial, 1, E)} of
                             {false, _} -> UpstreamPipelines;
                             {{value, {push_otp, false}}, _} -> UpstreamPipelines;

                             {{value, {push_otp, true}}, {value, {gitmaterial, Val}}} when Val /= [] ->
                                 {value, {name, Name}} = lists:keysearch(name, 1, E),
                                 [[{name, Name}]|UpstreamPipelines];
                             {{value, {push_otp, true}}, _} -> UpstreamPipelines
                        end
                 end, [], GrispData)
    }].
