-module(json_rec_tests).
-include_lib("eunit/include/eunit.hrl").

-record(simple, {
          one,
          two
          }).

-record(simplet2l, {
          two
          }).

-record(deep, {
          simple,
          second,
          third = null
         }).

-record(atoms, {
          enabled,
          disabled,
          nulled
          }).

-compile({parse_transform, exprecs}).
-export_records([simple,simplet2l,deep,atoms]).
-export([new/1]).

new(<<"simple">>) ->
    '#new-simple'();
new(<<"simplet2l">>) ->
    '#new-simplet2l'();
new(<<"deep">>) ->
    '#new-deep'();
new(<<"third">>) ->
    '#new-simple'();
new(<<"atoms">>) ->
    '#new-atoms'();
new(_RecName) -> undefined.

atoms_json_data() ->
    ["{\"enabled\":true, \"disabled\":false, \"nulled\":null}",
     #atoms{ enabled = true, disabled = false, nulled = null}].

simple_json_data() ->
    ["{\"one\":1,\"two\":2}",
     #simple{ one = 1, two = 2}].

simple_json_t2l_data() ->
    ["{\"two\":[1,2,3]}",
     #simplet2l{ two = [1,2,3]}].

unknown_json_data() ->
    ["{\"one\":1,\"two\":2}",
     [{<<"one">>, 1},{<<"two">>,2}]].

deep_json_data() ->
    Simple = "{\"simple\":{\"one\":1,\"two\":2}",
    Deep = Simple++"}",
    [Deep,
     #deep{ simple = #simple{ one = 1, two = 2}
            }].

deep_deep_json_data() ->
    Simple = "\"simple\":{\"one\":1,\"two\":2}",
    Deep = "{"++Simple++",\"second\":[{"++Simple ++ "},{" ++ Simple ++ "},{" ++ Simple ++"}],\"third\":{\"one\":1,\"two\":2}}",
    [Deep,
     #deep{ simple = #simple{ one = 1, two = 2},
            second  = [ #simple{ one = 1, two = 2},
                        #simple{ one = 1, two = 2},
                        #simple{ one = 1, two = 2}
                      ],
            third = #simple{ one = 1, two = 2}
          }
     ].

atoms_test() ->
    [Json, Rec] = atoms_json_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json), json_rec_tests, new(<<"atoms">>)),
    ?assertEqual(Rec, NewRec).

simple_test() ->
    [Json, Rec] = simple_json_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json),json_rec_tests,new(<<"simple">>)),
    ?assertEqual(Rec, NewRec).

simple_t2l_test() ->
    [Json, Rec] = simple_json_t2l_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json),json_rec_tests,new(<<"simplet2l">>)),
    ?assertEqual(Rec, NewRec).

deep_test() ->
    [Json, Rec] = deep_json_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json),json_rec_tests,new(<<"deep">>)),
    ?assertEqual(Rec, NewRec).

deep_deep_test()  ->
    [Json, Rec] = deep_deep_json_data(),
    New = json_rec:to_rec(jiffy:decode(Json),json_rec_tests,new(<<"deep">>)),
    ?assertEqual(Rec, New).

unknown_test() ->
    [Json, Rec] = unknown_json_data(),
    New = json_rec:to_rec(jiffy:decode(Json),json_rec_tests,new(<<"unknown">>)),
    ?assertEqual(Rec, New).

to_json_atoms_test() ->
    [_Json, Rec] = atoms_json_data(),

    Conv = json_rec:to_json(Rec, json_rec_tests),
    Sjson = jiffy:encode(Conv),

    New = json_rec:to_rec(jiffy:decode(Sjson), json_rec_tests, new(<<"atoms">>)),
    ?assertEqual(Rec, New).

to_json_simple_test() ->
    [_Json, Rec] = simple_json_data(),

    Conv = json_rec:to_json(Rec, json_rec_tests),
    Sjson= jiffy:encode(Conv),

    New = json_rec:to_rec(jiffy:decode(Sjson),json_rec_tests,new(<<"simple">>)),
    ?assertEqual(Rec,New).

to_json_simple_t2l_test() ->
    [_Json, Rec] = simple_json_t2l_data(),

    Conv = json_rec:to_json(Rec, json_rec_tests),
    Sjson= jiffy:encode(Conv),

    New = json_rec:to_rec(jiffy:decode(Sjson),json_rec_tests,new(<<"simplet2l">>)),
    ?assertEqual(Rec,New).

to_json_deep_test() ->
    [_Json, Rec] = deep_json_data(),
    Conv = json_rec:to_json(Rec,json_rec_tests),
    Sjson = jiffy:encode(Conv),

    New = json_rec:to_rec(jiffy:decode(Sjson), json_rec_tests, new(<<"deep">>)),
    ?assertEqual(Rec,New).

to_json_deep_deep_test() ->
    [_Json, Rec] = deep_deep_json_data(),
    Conv = json_rec:to_json(Rec,json_rec_tests),
    Sjson = jiffy:encode(Conv),
    
    New = json_rec:to_rec(jiffy:decode(Sjson), json_rec_tests, new(<<"deep">>)),
    ?assertEqual(Rec,New).


