-module(generate_pipelines_grisp).
-export([get_template_data/0]).

get_template_data() ->
    Config = get_full_config(config()),
    Permutations = get_permutations(Config),
    ResolvedName = resolve_name(Permutations),
    TemplateData = preprocess_template_data(ResolvedName),
    TemplateData.

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
              end}
      ]
     },
     {rebar3_grisp,
      [
       {version, default_otp_version()},
       {grisp, [master, hex]},
       {rebar3_grisp, {trigger, all_material_types()}},
       {grisp_tools, [master, hex]},
       {build_otp, [false, true]},
       {name, fun (TList) ->
                      Map = maps:from_list(TList),
                      {trigger, Rebar3_Grisp} = maps:get(rebar3_grisp, Map),
                      "rebar3-grisp-" ++ atom_to_list(Rebar3_Grisp) ++ "-grisp-" ++ atom_to_list(maps:get(grisp, Map)) ++
                          "-tools-" ++ atom_to_list(maps:get(grisp_tools, Map)) ++ "-build-" ++
                          atom_to_list(maps:get(build_otp, Map)) ++ "-otp-" ++
                          maps:get(version, Map)
              end}
      ]
     },
     {grisp_tools,
      [
       {version, default_otp_version()},
       {grisp, [master, hex]},
       {rebar3_grisp, [master, hex]},
       {grisp_tools, {trigger, all_material_types()}},
       {build_otp, [false, true]},
       {name, fun (TList) ->
                      Map = maps:from_list(TList),
                      {trigger, Grisp_Tools} = maps:get(grisp_tools, Map),
                      "tools-" ++ atom_to_list(Grisp_Tools) ++
                          "-grisp-" ++ atom_to_list(maps:get(grisp, Map)) ++ "-rebar-" ++
                          atom_to_list(maps:get(rebar3_grisp, Map)) ++ "-build-" ++ atom_to_list(maps:get(build_otp, Map)) ++
                          "-otp-" ++ maps:get(version, Map)
              end}
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
              end}
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
         {name, ""}
        ],
    Replaced = lists:map(fun ({Key, Val}) ->
                      case lists:keyfind(Key, 1, Conf) of
                          false -> {Key, Val};
                          E -> E
                      end
                         end, DefaultConfig),
    [{Type, Replaced} | get_full_config(T)];
get_full_config(Conf) -> Conf.


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

resolve_name(In) ->
    resolve_name(In, []).
resolve_name([], Resolved) ->
    Resolved;
resolve_name([H|T], Resolved) ->
    {value, {name, NameFun}} = lists:keysearch(name, 1, H),
    Pipeline = lists:keyreplace(name, 1, H, {name, NameFun(H)}),
    resolve_name(T, [Pipeline|Resolved]).

preprocess_template_data(Data) ->
    preprocess_template_data(Data, []).
preprocess_template_data([], Buf) ->
    Buf;
preprocess_template_data([H|T], Buf) ->
    Processed = lists:map(fun
                              ({grisp, {trigger, master}}) ->
                                  {gitmaterial, [
                                                 {url, "https://github.com/grisp/grisp.git"},
                                                 {destination, "grisp/"},
                                                 {name, "grisp"}
                                                ]};
                              ({grisp, {trigger, pr}}) ->
                                  {scmmaterial, [
                                                 {id, "7797947b-a4eb-458f-b655-e3a3017c6a07"},
                                                 {destination, "grisp/"},
                                                 {name, "grisp"}
                                               ]};
                              ({grisp, {trigger, fb}}) ->
                                  {scmmaterial, [
                                                 {id, "asdf123-e100-4e52-8ebf-a21193f47bf1"},
                                                 {destination, "grisp/"},
                                                 {name, "grisp"}
                                                ]};
                              ({grisp, master}) ->
                                  {checkout, [
                                              {url, "https://github.com/grisp/grisp.git"},
                                              {name, "grisp"},
                                              {destination, "grisp/"}
                                             ]};
                              ({rebar3_grisp, {trigger, master}}) ->
                                  {gitmaterial, [
                                                 {url, "https://github.com/grisp/rebar3_grisp.git"},
                                                 {destination, "rebar3_grisp/"},
                                                 {name, "rebar3_grisp"}
                                                ]};
                              ({rebar3_grisp, {trigger, pr}}) ->
                                  {scmmaterial, [
                                                 {id, "a0a55708-e100-4e52-8ebf-a21193f47bf1"},
                                                 {destination, "rebar3_grisp/"},
                                                 {name, "rebar3_grisp"}
                                                ]};
                              ({rebar3_grisp, {trigger, fb}}) ->
                                  {scmmaterial, [
                                                 {id, "as1123-e100-4e52-8ebf-a21193f47bf1"},
                                                 {destination, "rebar3_grisp/"},
                                                 {name, "rebar3_grisp"}
                                                ]};
                              ({rebar3_grisp, master}) ->
                                  {checkout, [
                                              {url,"https://github.com/grisp/rebar3_grisp.git"},
                                              {destination, "rebar3_grisp/"},
                                              {name, "rebar3_grisp"}
                                             ]};
                              ({grisp_tools, {trigger, master}}) ->
                                  {gitmaterial, [
                                                 {url, "https://github.com/grisp/grisp_tools.git"},
                                                 {destination, "grisp_tools/"},
                                                 {name, "grisp_tools"}
                                                ]};
                              ({grisp_tools, {trigger, pr}}) ->
                                 {scmmaterial, [
                                                {id, "b0a55708-e100-4e52-8ebf-a21193f47bf1"},
                                                {destination, "grisp_tools/"},
                                                {name, "grisp_tools"}
                                               ]};
                              ({grisp_tools, {trigger, fb}}) ->
                                  {scmmaterial, [
                                                 {id, "sfdf123-e100-4e52-8ebf-a21193f47bf1"},
                                                 {destination, "grisp_tools/"},
                                                 {name, "grisp_tools"}
                                                ]};
                              ({grisp_tools, master}) ->
                                  {checkout, [
                                              {url, "https://github.com/grisp/grisp_tools.git"},
                                              {destination, "grisp_tools/"},
                                              {name, "grisp_tools"}
                                             ]};
                              ({Key, true}) -> {Key, true};
                              ({Key, false}) -> {Key, false};
                              ({Key, Val}) when is_atom(Val) -> {Key, list_to_binary(atom_to_list(Val))};
                              (Else) -> Else
                          end, H),
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
    preprocess_template_data(T, [NewData|Buf]).
