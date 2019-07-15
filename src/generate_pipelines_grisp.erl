-module(generate_pipelines_grisp).
-export([get_template_data/0]).
-export([transform_dep/1]).
get_template_data() ->
    Config = get_full_config(config()),
    Permutations = generate_pipelines:get_permutations(Config),
    Graph = generate_pipelines:build_dep_graph(Permutations),
    Graph = generate_pipelines:vertex_map(fun (PipelineConfig, _Graph, _Vertex) ->
                                                        generate_pipelines:resolve_funs(PipelineConfig)
                                                end, Graph),
    generate_pipelines:vertex_map(fun
                                      (PipelineConfig, InGraph, Vertex) ->
                                          generate_pipelines:acc_template_data(
                                            PipelineConfig, InGraph, Vertex, fun generate_pipelines_grisp:transform_dep/1)
                                  end, Graph),
    generate_pipelines:graph_to_list(Graph).

all_otp_versions() ->
    [ "20.2", "21.0-rc1", "21.0", "22.0" ].
default_otp_version() ->
    [ "21.0" ].
all_material_types() ->
    [ master, fb, pr ].

config() ->
    [
     {grisp,
      [
       {version, all_otp_versions()},
       {grisp,  {trigger, all_material_types()}},
       {rebar3_grisp, false},
       {build_otp, true},
       {push_otp, true},
       {name, fun (TList) ->
                      Map = maps:from_list(TList),
                      {trigger, Grisp} = maps:get(grisp, Map),
                      "grisp-" ++ atom_to_list(Grisp) ++ "-otp-" ++ maps:get(version, Map)
              end},
       {group, grisp}
      ]
     },
     {rebar3_grisp,
      [
       {version, default_otp_version()},
       {grisp, [master, hex]},
       {rebar3_grisp, {trigger, all_material_types()}},
       {grisp_tools, [master, hex]},
       {build_otp, true},
       {name, fun (TList) ->
                      Map = maps:from_list(TList),
                      {trigger, Rebar3_Grisp} = maps:get(rebar3_grisp, Map),
                      "rebar3-grisp-" ++ atom_to_list(Rebar3_Grisp) ++ "-grisp-" ++
                          atom_to_list(maps:get(grisp, Map)) ++
                          "-tools-" ++ atom_to_list(maps:get(grisp_tools, Map)) ++ "-build-" ++
                          atom_to_list(maps:get(build_otp, Map)) ++ "-otp-" ++
                          maps:get(version, Map)
              end},
       {group, rebar3_grisp},
       {downstream, [{build_otp, false}]}
      ]
     },
     {grisp_tools,
      [
       {version, default_otp_version()},
       {grisp, [master, hex]},
       {rebar3_grisp, [master, hex]},
       {grisp_tools, {trigger, all_material_types()}},
       {build_otp, true},
       {name, fun (TList) ->
                      Map = maps:from_list(TList),
                      {trigger, Grisp_Tools} = maps:get(grisp_tools, Map),
                      "tools-" ++ atom_to_list(Grisp_Tools) ++
                          "-grisp-" ++ atom_to_list(maps:get(grisp, Map)) ++ "-rebar-" ++
                          atom_to_list(maps:get(rebar3_grisp, Map)) ++ "-build-" ++ atom_to_list(maps:get(build_otp, Map)) ++
                          "-otp-" ++ maps:get(version, Map)
              end},
       {group, grisp_tools},
       {downstream, [{build_otp, false}]}
      ]
     },
     {new_toolchain,
      [
       {version, all_otp_versions()},
       {fetch_toolchain, true},
       {build_otp, true},
       {push_toolchain, true},
       {name, fun (TList) ->
                      Map = maps:from_list(TList),
                      "new-toolchain-otp-" ++ maps:get(version, Map)
              end},
       {group, new_toolchain}
      ]
     }
    ].

get_full_config([{Type, Conf}|T]) ->
   DefaultConfig =
        [
         {version, default_otp_version()},
         {grisp, hex},
         {rebar3_grisp, hex},
         {grisp_tools, hex},
         {fetch_toolchain, false},
         {build_otp, false},
         {push_toolchain, false},
         {push_otp, false},
         {name, ""},
         {group, ""},
         {downstream, []}
        ],
    Replaced = lists:map(fun ({Key, Val}) ->
                                 case lists:keyfind(Key, 1, Conf) of
                                     false -> {Key, Val};
                                     E -> E
                                 end
                         end, DefaultConfig),
    [{Type, Replaced} | get_full_config(T)];
get_full_config(Conf) -> Conf.

transform_dep({grisp, {trigger, master}}) ->
    {gitmaterial, [
                   {url, "https://github.com/grisp/grisp.git"},
                   {destination, "grisp/"},
                   {name, "grisp"}
                  ]};
transform_dep({grisp, {trigger, pr}}) ->
    {scmmaterial, [
                   {id, "7797947b-a4eb-458f-b655-e3a3017c6a07"},
                   {destination, "grisp/"},
                   {name, "grisp"}
                  ]};
transform_dep({grisp, {trigger, fb}}) ->
    {scmmaterial, [
                   {id, "asdf123-e100-4e52-8ebf-a21193f47bf1"},
                   {destination, "grisp/"},
                   {name, "grisp"}
                  ]};
transform_dep({grisp, master}) ->
    {checkout, [
                {url, "https://github.com/grisp/grisp.git"},
                {name, "grisp"},
                {destination, "grisp/"}
               ]};
transform_dep({rebar3_grisp, {trigger, master}}) ->
    {gitmaterial, [
                   {url, "https://github.com/grisp/rebar3_grisp.git"},
                   {destination, "rebar3_grisp/"},
                   {name, "rebar3_grisp"}
                  ]};
transform_dep({rebar3_grisp, {trigger, pr}}) ->
    {scmmaterial, [
                   {id, "a0a55708-e100-4e52-8ebf-a21193f47bf1"},
                   {destination, "rebar3_grisp/"},
                   {name, "rebar3_grisp"}
                  ]};
transform_dep({rebar3_grisp, {trigger, fb}}) ->
    {scmmaterial, [
                   {id, "as1123-e100-4e52-8ebf-a21193f47bf1"},
                   {destination, "rebar3_grisp/"},
                   {name, "rebar3_grisp"}
                  ]};
transform_dep({rebar3_grisp, master}) ->
    {checkout, [
                {url,"https://github.com/grisp/rebar3_grisp.git"},
                {destination, "rebar3_grisp/"},
                {name, "rebar3_grisp"}
               ]};
transform_dep({grisp_tools, {trigger, master}}) ->
    {gitmaterial, [
                   {url, "https://github.com/grisp/grisp_tools.git"},
                   {destination, "grisp_tools/"},
                   {name, "grisp_tools"}
                  ]};
transform_dep({grisp_tools, {trigger, pr}}) ->
    {scmmaterial, [
                   {id, "b0a55708-e100-4e52-8ebf-a21193f47bf1"},
                   {destination, "grisp_tools/"},
                   {name, "grisp_tools"}
                  ]};
transform_dep({grisp_tools, {trigger, fb}}) ->
    {scmmaterial, [
                   {id, "sfdf123-e100-4e52-8ebf-a21193f47bf1"},
                   {destination, "grisp_tools/"},
                   {name, "grisp_tools"}
                  ]};
transform_dep({grisp_tools, master}) ->
    {checkout, [
                {url, "https://github.com/grisp/grisp_tools.git"},
                {destination, "grisp_tools/"},
                {name, "grisp_tools"}
               ]};
transform_dep({Key, true}) -> {Key, true};
transform_dep({Key, false}) -> {Key, false};
transform_dep({Key, Val}) when is_atom(Val) -> {Key, list_to_binary(atom_to_list(Val))};
transform_dep(Else) -> Else.
