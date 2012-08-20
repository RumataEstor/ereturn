-module(ereturn_example2).

-export([do/1]).

-import(erlang, [return/1]).

-compile([{parse_transform, ereturn}]).


do(Param) ->
    case Param of
        _ when Param < 0 ->
            io:format("Negative\n"),
            return(ok);
        _ when Param > 0 ->
            case Param rem 2 of
                1 ->
                    io:format("Positive odd\n");
                _ ->
                    io:format("Positive even\n")
            end;
        0 ->
            io:format("Zero\n")
    end,
    io:format("Finish\n").
