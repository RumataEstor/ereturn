-module(simple).

-export([tests/0]).

-import(erlang, [return/1]).

-compile([{parse_transform, ereturn}]).

simple(_, F) ->
    F(start),
    return(return),
    F(after_return),
    result.

tests() ->
    [{fun simple/2,
      [{undefined, [start], return}
      ]}].
