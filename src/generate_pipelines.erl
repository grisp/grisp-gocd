-module(generate_pipelines).

%% API exports
-export([main/1]).
-export([
         get_permutations/1,
         build_dep_graph/1,
         resolve_funs/1,
         acc_template_data/4,
         vertex_map/2,
         graph_to_list/1
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

vertex_map(Fun, Graph) ->
    VerticesList = digraph:vertices(Graph),
    lists:map(fun (Vertex) ->
                      {Vertex, Label} = digraph:vertex(Graph, Vertex),
                      digraph:add_vertex(Graph, Vertex, Fun(Label, Graph, Vertex))
              end, VerticesList),
    Graph.

graph_to_list(Graph) ->
    lists:map(fun (Vertex) ->
                      {Vertex, Label} = digraph:vertex(Graph, Vertex),
                      Label
              end,
              digraph:vertices(Graph)).

render_template(TemplateFile, Data) ->
    Template = bbmustache:parse_file(TemplateFile),
    Rendered = bbmustache:compile(Template, Data, [{key_type, atom}]),
    %% did we produce valid JSON?
    Decoded = jsone:decode(Rendered),
    Pretty = jsone:encode(Decoded, [
                                    {indent, 2},
                                    {space, 1},
                                    native_utf8,
                                    native_forward_slash,
                                    {float_format, [{decimals, 2}, compact]}
                                   ]),
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
get_permutations_type([{downstream, Vals}|T]) ->
    [[{downstream, Vals}|B] || B <- get_permutations_type(T)];
get_permutations_type([{Key, Vals}|T]) when is_list(Vals) ->
    [ [{Key, Val}|B] || Val <- Vals, B <- get_permutations_type(T) ];
get_permutations_type([{Key, Tuple}|T]) when is_tuple(Tuple) ->
    {DepType, Vals} = Tuple,
    [ [{Key, {DepType, Val}}|B] || Val <- Vals, B <- get_permutations_type(T)];
get_permutations_type([{Key, Val}|T]) ->
    [ [{Key, Val}|B] || B <- get_permutations_type(T) ].

build_dep_graph(Config) ->
    %% put everything into a digraph as a vertice
    G = digraph:new([acyclic]),
    lists:map(fun (Conf) ->
                      V = digraph:add_vertex(G),
                      case lists:keyfind(downstream, 1, Conf) of
                          {downstream, DSList} ->
                              UpstreamV = digraph:add_vertex(G, V, Conf),
                              %% Add vertex and edge for downstream pipeline
                              lists:map(fun ({Key, Val}) ->
                                                %% same config just with replaced {key, val}
                                                io:format("strewe"),
                                                ConfDS = lists:keyreplace(Key, 1, Conf, {Key, Val}),
                                                DownstreamV = digraph:add_vertex(G),
                                                DownstreamV = digraph:add_vertex(G, DownstreamV, ConfDS),
                                                digraph:add_edge(G, UpstreamV, DownstreamV),
                                                ok
                                        end, DSList),
                              ok;
                          false ->
                              digraph:add_vertex(G, V, Conf),
                              ok
                      end
              end, Config),
    G.

resolve_funs(Config) ->
    lists:map(fun
                  ({Key, Val}) when is_function(Val) ->
                      io:format("Name ~p~n", [Val(Config)]),
                      {Key, Val(Config)};
                  (Else) -> Else
              end, Config).

acc_template_data(Config, Graph, Vertex, Fun) ->
    Processed = lists:map(Fun, Config),
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
    UpstreamPipelines = lists:map(fun (Edge) ->
                                          io:format("yolo"),
                                          {Edge, FromV, _ToV, _Label} = digraph:edge(Graph, Edge),
                                          {FromV, UpstreamData} = digraph:vertex(Graph, FromV),
                                          [lists:keyfind(name, 1, UpstreamData)]
                                  end, digraph:in_edges(Graph, Vertex)),
    Removed ++ [{gitmaterial, Gitmaterials}] ++
        [{scmmaterial, Scmmaterials}] ++
        [{checkout, Checkouts}] ++ [{upstream_pipeline, UpstreamPipelines}].
