-module(fun_simple).

-export([tests/0]).

-import(erlang, [return/1]).
-compile([{parse_transform, ereturn}]).


fun_simple(Param, F) ->
    F("start"),
    Fun = fun(V) ->
                  F("in fun"),
                  if V =:= 0 ->
                          F(0);
                     V =:= 1 ->
                          F(1),
                          return({fun_return, V})
                  end,
                  {result, V}
          end,
    Fun(Param).


tests() ->
    [{fun fun_simple/2,
      [{0, ["start", "in fun", 0], {result, 0}},
       {1, ["start", "in fun", 1], {fun_return, 1}}
      ]}
    ].
