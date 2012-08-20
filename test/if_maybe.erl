-module(if_maybe).

-export([tests/0]).

-import(erlang, [return/1]).
-compile([{parse_transform, ereturn}]).


if_maybe_return(Param, F) ->
    F(start),
    if Param =:= 0 ->
            F(0);
       Param =:= 1 ->
            F(1),
            erlang:return({return, 1});
       Param =:= 2 ->
            F(2),
            return({return, 2});
       true ->
            F("_"),
            case Param of
                3 ->
                    F(3),
                    return({return, 3});
                _ ->
                    F("__"),
                    case Param of
                        4 ->
                            F(4);
                        _ ->
                            F("not 4")
                    end
            end
    end,
    F(after_case),
    {result, Param}.


tests() ->
    [{fun if_maybe_return/2,
      [{0, [start, 0, after_case], {result, 0}},
       {1, [start, 1], {return, 1}},
       {2, [start, 2], {return, 2}},
       {3, [start, "_", 3], {return, 3}},
       {4, [start, "_", "__", 4, after_case], {result, 4}},
       {5, [start, "_", "__", "not 4", after_case], {result, 5}}
      ]}
    ].
