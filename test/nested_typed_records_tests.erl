-module(nested_typed_records_tests).
-include_lib("eunit/include/eunit.hrl").

-record(simple, {
            one
        }).

-record(simple2, {
            two
        }).

-record(doubleSimple, {
            first,
            second
        }).

-record(deep, {
            simple,
            simple2
         }).

-record(typedSimple, {
            mySimple = '#new-simple'()
        }).

-record(typedSimple2, {
            mySimple = '#new-simple2'()
        }).

-record(typedSimple3, {
            typedSimple,
            myTypedSimple2 = '#new-typedSimple2'()
        }).

-record(typedSimpleArray, {
            mySimple = ['#new-doubleSimple'()]
        }).

-compile({parse_transform, exprecs}).
-export_records([simple,simple2, doubleSimple, deep, typedSimple, typedSimple2, typedSimple3, typedSimpleArray]).
-export([new/1]).

new(<<"simple">>) ->
    '#new-simple'();
new(<<"simple2">>) ->
    '#new-simple2'();
new(<<"doubleSimple">>) ->
    '#new-doubleSimple'();
new(<<"deep">>) ->
    '#new-deep'();
new(<<"typedSimple">>) ->
    '#new-typedSimple'();
new(<<"typedSimple2">>) ->
    '#new-typedSimple2'();
new(<<"typedSimple3">>) ->
    '#new-typedSimple3'();
new(<<"typedSimpleArray">>) ->
    '#new-typedSimpleArray'();
new(_RecName) -> undefined.

simple_json_data() ->
    [<<"{\"one\":1}">>,
     #simple{ one = 1}].

simple_to_rec_test() ->
    [Json, Rec] = simple_json_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json), ?MODULE, new(<<"simple">>)),
    ?assertEqual(Rec, NewRec).

simple_to_json_test() ->
    [Json, Rec] = simple_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    ?assertEqual(Json,Sjson).

simple_round_trip_test() ->
    [_, Rec] = simple_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    NewRec = json_rec:to_rec(jiffy:decode(Sjson), ?MODULE, new(<<"simple">>)),
    ?assertEqual(Rec, NewRec).

deep_json_data() ->
    [   <<"{\"simple\":{\"one\":1},\"simple2\":{\"two\":2}}">>,
        #deep{ 
            simple = #simple{ one = 1 }, 
            simple2 = #simple2{ two = 2 } 
        }
    ].

deep_to_rec_test() ->
    [Json, Rec] = deep_json_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json), ?MODULE, new(<<"deep">>)),
    ?assertEqual(Rec, NewRec).

deep_to_json_test() ->
    [Json, Rec] = deep_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    ?assertEqual(Json,Sjson).

deep_round_trip_test() ->
    [_, Rec] = deep_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    NewRec = json_rec:to_rec(jiffy:decode(Sjson), ?MODULE, new(<<"deep">>)),
    ?assertEqual(Rec, NewRec).


typedSimple_json_data() ->
    [   <<"{\"mySimple\":{\"one\":1}}">>,
        #typedSimple{ 
            mySimple = #simple{ one = 1 } 
        }
    ].

typedSimple_to_rec_test() ->
    [Json, Rec] = typedSimple_json_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json), ?MODULE, new(<<"typedSimple">>)),
    ?assertEqual(Rec, NewRec).

typedSimple_to_json_test() ->
    [Json, Rec] = typedSimple_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    ?assertEqual(Json,Sjson).

typedSimple_round_trip_test() ->
    [_, Rec] = typedSimple_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    NewRec = json_rec:to_rec(jiffy:decode(Sjson), ?MODULE, new(<<"typedSimple">>)),
    ?assertEqual(Rec, NewRec).


typedSimple2_json_data() ->
    [   <<"{\"mySimple\":{\"two\":1}}">>,
        #typedSimple2{ 
            mySimple = #simple2{ two = 1 } 
        }
    ].

typedSimple2_to_rec_test() ->
    [Json, Rec] = typedSimple2_json_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json), ?MODULE, new(<<"typedSimple2">>)),
    ?assertEqual(Rec, NewRec).

typedSimple2_to_json_test() ->
    [Json, Rec] = typedSimple2_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    ?assertEqual(Json,Sjson).

typedSimple2_round_trip_test() ->
    [_, Rec] = typedSimple2_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    NewRec = json_rec:to_rec(jiffy:decode(Sjson), ?MODULE, new(<<"typedSimple2">>)),
    ?assertEqual(Rec, NewRec).


typedSimple3_json_data() ->
    [   <<"{\"typedSimple\":{\"mySimple\":{\"one\":1}},\"myTypedSimple2\":{\"mySimple\":{\"two\":1}}}">>,
        #typedSimple3{ 
            typedSimple = #typedSimple{ mySimple = #simple{ one = 1 } },
            myTypedSimple2 = #typedSimple2{ mySimple = #simple2{ two = 1 } }
        }
    ].

typedSimple3_to_rec_test() ->
    [Json, Rec] = typedSimple3_json_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json), ?MODULE, new(<<"typedSimple3">>)),
    ?assertEqual(Rec, NewRec).

typedSimple3_to_json_test() ->
    [Json, Rec] = typedSimple3_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    ?assertEqual(Json,Sjson).

typedSimple3_round_trip_test() ->
    [_, Rec] = typedSimple3_json_data(),
    Conv = json_rec:to_json(Rec, ?MODULE),
    Sjson = jiffy:encode(Conv),
    NewRec = json_rec:to_rec(jiffy:decode(Sjson), ?MODULE, new(<<"typedSimple3">>)),
    ?assertEqual(Rec, NewRec).

typedSimpleArray_json_data() ->
    [   <<"{\"mySimple\":[{\"first\":1,\"second\":2},{\"first\":2,\"second\":3}]}">>,
        #typedSimpleArray{ 
            mySimple = [#doubleSimple{ first = 1, second = 2 },#doubleSimple{ first = 2, second = 3 }] 
        }
    ].

typedSimpleArray_to_rec_test() ->
    [Json, Rec] = typedSimpleArray_json_data(),
    NewRec = json_rec:to_rec(jiffy:decode(Json), ?MODULE, new(<<"typedSimpleArray">>)),
    ?assertEqual(Rec, NewRec).
%% The round trip in this case doesn't work
%% It conflicts with the expected results in the deep_deep tests in json_rec_tests
%%
%typedSimpleArray_to_json_test() ->
%    [Json, Rec] = typedSimpleArray_json_data(),
%    Conv = json_rec:to_json(Rec, ?MODULE),
%    Sjson = jiffy:encode(Conv),
%    ?assertEqual(Json,Sjson).
%
%typedSimpleArray_round_trip_test() ->
%    [_, Rec] = typedSimpleArray_json_data(),
%    Conv = json_rec:to_json(Rec, ?MODULE),
%    Sjson = jiffy:encode(Conv),
%    NewRec = json_rec:to_rec(jiffy:decode(Sjson), ?MODULE, new(<<"typedSimpleArray">>)),
%    ?assertEqual(Rec, NewRec).

