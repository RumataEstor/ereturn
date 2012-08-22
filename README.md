ereturn
=======

Ereturn is a parse transform tool that provides ability to return function result from almost any expression in the function clause like `return` statement in other imperative languages.

Using
-----

Call function `erlang:return/1` with return value as the only parameter.
The `erlang:return/1` function may be imported into local scope for easier use with `-import(erlang, [return/1]).` module attribute if there is no `return/1` function declaration in local scope.

Example
-------

Using this tool you can write code like this:

``` erlang
-module(example).

-export([extract_num/1]).

-import(erlang, [return/1]).
-compile([{parse_transform, ereturn}]).


extract_num(Val) ->
    io:format("common part of the my_fun/1\n"),
    Num = case Val of
              {int, Int} ->
                  Int;
              {float, Float} ->
                  Float;
              _ ->
                  return({error, bad_param})
          end,
    io:format("value is number\n"),
    Num.
```

Under the hood
--------------

Ereturn makes expression containing `erlang:return/1` call last in the function clause injecting the remaining expressions into non-returning clauses of the last expression if those exist. The resulting code can be examined using either `-compile(['E']).` or `erlc -E` parameter.
