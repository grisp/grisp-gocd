-module(generate_pipelines).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(_Args) ->
    GrispTemplateData = generate_pipelines_grisp:get_template_data(),
    DeployOTPTemplateData = generate_pipelines_deploy_otp:get_template_data(GrispTemplateData),
    DeployToolchainTemplateData = generate_pipelines_deploy_toolchain:get_template_data(GrispTemplateData),

    io:format("Template Data: ~n~p~n~p~n~p~n",
              [GrispTemplateData, DeployOTPTemplateData, DeployToolchainTemplateData]),

    Templates =
        [{"deploy-otp-to-s3", render_template(<<"./priv/deploy-otp-to-s3.mustache.gopipeline.json">>, DeployOTPTemplateData)}] ++
        [{"deploy-toolchain-to-s3", render_template(<<"./priv/deploy-toolchain-to-s3.mustache.gopipeline.json">>, DeployToolchainTemplateData)}] ++
        lists:map(fun (Vals) ->
                          {value, {name, Name}} = lists:keysearch(name, 1, Vals),
                          {Name, render_template(<<"./priv/grisp.mustache.gopipeline.json">>, Vals)}
                  end, GrispTemplateData),

    lists:map(fun ({Name, Val}) ->
                      file:write_file("grisp-gocd-config/" ++ Name ++ ".gopipeline.json", Val) end, Templates),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

render_template(TemplateFile, Data) ->
    Template = bbmustache:parse_file(TemplateFile),
    Rendered = bbmustache:compile(Template, Data, [{key_type, atom}]),
    %% did we produce valid JSON?
    %io:format("~s~n", [Rendered]),
    Decoded = jsone:decode(Rendered),
    Pretty = jsone:encode(Decoded, [
                                    {indent, 2},
                                    {space, 1},
                                    native_utf8,
                                    native_forward_slash,
                                    {float_format, [{decimals, 2}, compact]}
                                   ]),
    %io:format("~s~n", [Pretty]),
    Pretty.
