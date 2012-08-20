-module(case_maybe).

-export([tests/0]).

-import(erlang, [return/1]).
-compile([{parse_transform, ereturn}]).


cases_maybe_return(Param, F) ->
    F(start),
    case Param of
        0 ->
            F(0);
        1 ->
            F(1),
            erlang:return({return, 1});
        2 ->
            F(2),
            return({return, 2});
        _ ->
            F("_"),
            case Param of
                3 ->
                    F(3),
                    return({return, 3});
                _ ->
                    F("__")
            end,
            F("after inner case"),
            ok
    end,
    F(after_case),
    {result, Param}.


tests() ->
    [{fun cases_maybe_return/2,
      [{0, [start, 0, after_case], {result, 0}},
       {1, [start, 1], {return, 1}},
       {2, [start, 2], {return, 2}},
       {3, [start, "_", 3], {return, 3}},
       {4, [start, "_", "__", "after inner case", after_case], {result, 4}}
      ]}
    ].
