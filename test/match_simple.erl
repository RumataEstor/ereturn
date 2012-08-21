-module(match_simple).

-export([tests/0]).

-import(erlang, [return/1]).
-compile([{parse_transform, ereturn}]).


match_simple(Param, F) ->
    F("start"),
    case Param of
        0 ->
            F(0),
            Val = zero;
        _ when Param rem 2 =:= 1 ->
            F(odd),
            Val = case Param of
                      1 ->
                          F(1),
                          5;
                      _ ->
                          return({return, Param})
                  end,
            F(Val);
        _ ->
            F(even),
            return({return, Param})
    end,
    {result, Val}.


tests() ->
    [{fun match_simple/2,
      [{0, ["start", 0], {result, zero}},
       {1, ["start", odd, 1, 5], {result, 5}},
       {2, ["start", even], {return, 2}},
       {3, ["start", odd], {return, 3}}
      ]}
    ].
