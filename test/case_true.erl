-module(case_true).

-export([tests/0]).

-import(erlang, [return/1]).
-compile([{parse_transform, ereturn}]).


case_full_return(Param, F) ->
    F(start),
    case Param of
        0 ->
            F(0);
        _ ->
            F("_"),
            case Param of
                1 ->
                    return({return, 1});
                2 ->
                    return({return, 2})
            end,
            F("after _")
    end,
    F(finish),
    {result, Param}.

tests() ->
    [{fun case_full_return/2,
      [{0, [start, 0, finish], {result, 0}},
       {1, [start, "_"], {return, 1}},
       {2, [start, "_"], {return, 2}},
       {3, [start, "_", "after _", finish], {result, 3}}
      ]}].
