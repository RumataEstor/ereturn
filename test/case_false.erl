-module(case_false).

-export([tests/0]).

-import(erlang, [return/1]).
-compile([{parse_transform, ereturn}]).


case_no_return(Param, F) ->
    F(start),
    case Param of
        0 ->
            F(0),
            return({return, 0}),
            F("after 0");
        _ ->
            F("_"),
            case Param of
                1 ->
                    F(1);
                2 ->
                    F(2)
            end
    end,
    F(finish),
    {result, Param}.

tests() ->
    [{fun case_no_return/2,
      [{0, [start, 0], {return, 0}},
       {1, [start, "_", 1, finish], {result, 1}},
       {2, [start, "_", 2, finish], {result, 2}},
       {3, [start, "_"], {'EXIT', 'Pid', '_'}}
      ]}].
