-module(generate_pipelines).

%% API exports
-export([main/1]).
-export([
         get_permutations/1,
         resolve_funs/1,
         acc_template_data/2
        ]).

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
get_permutations(Config) ->
    get_permutations(Config, []).
get_permutations([], Acc) ->
    Acc;
get_permutations([H|T], Acc) ->
    {_Type, Config} = H,
    Perms = get_permutations_type(Config),
    get_permutations(T, Perms ++ Acc).

get_permutations_type([]) -> [[]];
get_permutations_type([{Key, Vals}|T]) when is_list(Vals) ->
    [ [{Key, Val}|B] || Val <- Vals, B <- get_permutations_type(T) ];
get_permutations_type([{Key, Tuple}|T]) when is_tuple(Tuple) ->
    {DepType, Vals} = Tuple,
    [ [{Key, {DepType, Val}}|B] || Val <- Vals, B <- get_permutations_type(T) ];
get_permutations_type([{Key, Val}|T]) ->
    [ [{Key, Val}|B] || B <- get_permutations_type(T) ].

resolve_funs(In) ->
    resolve_funs(In, []).
resolve_funs([], Resolved) ->
    Resolved;
resolve_funs([H|T], Resolved) ->
    Pipeline = lists:map(fun
                  ({Key, Val}) when is_function(Val) ->
                      {Key, Val(H)};
                  (Else) -> Else
              end, H),
    resolve_funs(T, [Pipeline|Resolved]).

acc_template_data(Data, Fun) ->
    acc_template_data(Data, [], Fun).
acc_template_data([], Buf, _Fun) ->
    Buf;
acc_template_data([H|T], Buf, Fun) ->
    Processed = lists:map(Fun, H),
    Gitmaterials = lists:foldl(fun
                                   ({gitmaterial, Val}, Acc) -> [Val|Acc];
                                   (_Other, Acc) -> Acc
                               end, [], Processed),
    Scmmaterials = lists:foldl(fun
                                   ({scmmaterial, Val}, Acc) -> [Val|Acc];
                                   (_Other, Acc) -> Acc
                               end, [], Processed),
    Checkouts = lists:foldl(fun
                                ({checkout, Val}, Acc) -> [Val|Acc];
                                (_Other, Acc) -> Acc
                            end, [], Processed),
    Removed = lists:filter(fun
                               ({gitmaterial, _Val}) -> false;
                               ({scmmaterial, _Val}) -> false;
                               ({checkout, _Val}) -> false;
                               (_Else) -> true
                          end, Processed),
    NewData = Removed ++ [{gitmaterial, Gitmaterials}] ++
        [{scmmaterial, Scmmaterials}] ++
        [{checkout, Checkouts}],
    acc_template_data(T, [NewData|Buf], Fun).
