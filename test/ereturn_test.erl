-module(ereturn_test).

-include_lib("eunit/include/eunit.hrl").


return_test_() ->
    {inparallel,
     [[[?_test(do_test({fun(F) -> TestFun(P, F) end, S, R}))
        || {P, S, R} <- ParamStatesList]
       || {TestFun, ParamStatesList} <- M:tests()]
      || M <- [simple, case_true, case_maybe, case_false, if_maybe, fun_simple]]
    }.


do_test({TestFun, States, Result}) ->
    erlang:process_flag(trap_exit, true),
    Self = self(),
    Pid = spawn_link(fun() ->
                             SignalFun = fun(I) -> Self ! {self(), I} end,
                             R = TestFun(SignalFun),
                             SignalFun(R)
                     end),
    expect_states(States ++ [Result], Pid).


expect_states([S | Next], Pid) ->
    receive
        Msg ->
            ?assertEqual({Pid, S}, Msg)
    end,
    expect_states(Next, Pid);
expect_states([], Pid) ->
    receive
        Msg ->
            ?assertEqual({'EXIT', Pid, normal}, Msg)
    end.
