-module(ereturn).

-export([parse_transform/2]).


-ifdef(DEBUG).
-define(DEBUG(Fmt, Params), io:format(Fmt, Params)).
-else.
-define(DEBUG(Fmt, Params), case get(debug) of true -> io:format(Fmt, Params); _ -> ok end).
-endif.

-record(state, {filename,
                only_remote = true,
                return = false :: false | true | maybe,
                no_support = false
               }).


parse_transform(Forms, _Options) ->
    ?DEBUG("Before: ~p\n", [Forms]),
    try
        {NewForms, State} = mapfoldl(fun top/2, #state{}, Forms),
        parse_trans_pp:pp_src(NewForms, State#state.filename ++ ".xfm"),
        NewForms
    catch
        throw:{error, _, _} = Reason ->
            throw(Reason);
        _:Reason ->
            [{_, _, _, Opts} | _] = Stacktrace = erlang:get_stacktrace(),
            throw({error, [{"ereturn.erl",
                    [{proplists:get_value(line, Opts), compile,
                      {parse_transform, ?MODULE,
                       {Reason, Stacktrace}}}]}], []})
    end.


top({attribute, L, import, {erlang, Items}} = Form, State) ->
    case lists:partition(fun({return, 1}) -> true;
                            (_) -> false end, Items) of
        {[], _} ->
            {Form, State};
        {[_|_], []} ->
            {[], State#state{only_remote=false}};
        {_, NewItems} ->
            {{attribute, L, import, {erlang, NewItems}},
             State#state{only_remote=false}}
    end;
top({attribute, _, file, {Filename, _}} = Form, State) ->
    {Form, State#state{filename=Filename}};
top({function, L, return, _, _}, #state{only_remote=false}=State) ->
    msg_error(L, State, "local return function clashes with imported");
top({function, L, Name, Arity, Clauses}, State) ->
    {NewClauses, _} = mapfoldl(fun clause/2, State, Clauses),
    ?DEBUG("Before cleaning:\n\t~p\n", [NewClauses]),
    LastClauses = clean_clauses(NewClauses),
    {{function, L, Name, Arity, LastClauses}, State};
top(Form, State) ->
    {Form, State}.


clause({clause, L, Params, Guards, Expressions}, State) ->
    {NewExpressions, NewState} = expr(Expressions, clean(State), []),
    {{NewState#state.return, {clause, L, Params, Guards, NewExpressions}}, NewState}.


expr([], State, Acc) ->
    {Acc, State};

expr(_Next, #state{return=true} = State, Acc) ->
    {Acc, State};

expr(Next, #state{return=maybe} = State, Acc) ->
    {After, AfterState} = expr(Next, clean(State), []),
    NewAcc = apply_after_expressions(Acc, fun(Last) -> After ++ [Last] end),
    expr([], combine(State#state{return=maybe}, AfterState), NewAcc);

expr([{call, L, Fn, Parameters} = Form | Next], State, Acc) ->
    ?DEBUG("call ~p, state ~p\n", [Form, State]),
    check_no_return(Parameters, State),
    IsReturn = case Fn of
                   {remote, _, {atom, _,erlang}, {atom, _, return}} when
                         length(Parameters) =:= 1 ->
                       true;
                   {atom, _, return} when
                         length(Parameters) =:= 1 ->
                       ?DEBUG("return found, state ~p\n", [State]),
                       not State#state.only_remote;
                   _ ->
                       false
               end,
    case IsReturn of
        false ->
            expr(Next, State, [Form | Acc]);
        true when State#state.no_support =/= false ->
            not_supported(L, State);
        true ->
            expr([], State#state{return=true}, Parameters ++ Acc)
    end;

expr([{'case', L, Expression, Clauses} | Next], State, Acc) ->
    check_no_return([Expression], State),
    {NewState, NewClauses} = control_clauses(State, Clauses),
    expr(Next, NewState, [{'case', L, Expression, NewClauses} | Acc]);

expr([{'if', L, Clauses} | Next], State, Acc) ->
    {NewState, NewClauses} = control_clauses(State, Clauses),
    expr(Next, NewState, [{'if', L, NewClauses} | Acc]);

expr([{match, L, LValue, RValue} | Next], State, Acc) ->
    {[NewLValue], _} = check_no_return([LValue], State),
    case expr([RValue], State, []) of
        {[NewRValue], #state{return=false}} ->
            expr(Next, State, [{match, L, NewLValue, NewRValue} | Acc]);
        {[NewRValue], #state{return=true} = NewState} ->
            expr([], NewState, [NewRValue | Acc]);
        {NewRExpr, NewState} ->
            Forms = apply_after_expressions(NewRExpr, fun(Last) ->
                                                              [{match, L, NewLValue, Last}]
                                                      end),
            expr(Next, NewState, Forms ++ Acc)
    end;

expr([{lc, _, Expr, _Generator} = Form | Next], State, Acc) ->
    %%
    %% TODO
    %%
    check_no_return([Expr], State),
    expr(Next, State, [Form | Acc]);

expr([{'catch', _, Expr} = Form | Next], State, Acc) ->
    %%
    %% TODO
    %%
    check_no_return([Expr], State),
    expr(Next, State, [Form | Acc]);

expr([{'fun', L, {clauses, Clauses}} | Next], State, Acc) ->
    ?DEBUG("fun found, state: ~p\n", [State]),
    {NewClauses, _} = mapfoldl(fun clause/2, State#state{no_support=false}, Clauses),
    ?DEBUG("new clauses ~p\n", [NewClauses]),
    expr(Next, State, [{'fun', L, {clauses, NewClauses}} | Acc]);

expr([{'fun', _, {function, M, F, A}} = Form | Next], State, Acc) ->
    check_no_return([M, F, A], State),
    expr(Next, State, [Form | Acc]);

expr([{block, _, Expressions} = Form | Next], State, Acc) ->
    %%
    %% TODO
    %%
    check_no_return(Expressions, State),
    expr(Next, State, [Form | Acc]);

expr([{'try', _, Expressions, SuccessClauses, ExceptionClauses, []} = Form | Next], State, Acc) ->
    %%
    %% TODO
    %%
    check_no_return(Expressions, State),
    mapfoldl(fun clause/2, State#state{no_support=Form}, SuccessClauses),
    mapfoldl(fun clause/2, State#state{no_support=Form}, ExceptionClauses),
    expr(Next, State, [Form | Acc]);

expr([{'receive', _, Clauses} = Form | Next], State, Acc) ->
    %%
    %% TODO
    %%
    mapfoldl(fun clause/2, State#state{no_support=Form}, Clauses),
    expr(Next, State, [Form | Acc]);

expr([{'receive', _, Clauses, Timeout, AfterExpressions} = Form | Next], State, Acc) ->
    %%
    %% TODO
    %%
    check_no_return([Timeout | AfterExpressions], State),
    mapfoldl(fun clause/2, State#state{no_support=Form}, Clauses),
    expr(Next, State, [Form | Acc]);

expr([{record, _, _Name, Fields} = Form | Next], State, Acc) ->
    [check_no_return([Expr], State) || {record_field, _, _Field, Expr} <- Fields],
    expr(Next, State, [Form | Acc]);

expr([{record, _, Object, _Name, Fields} = Form | Next], State, Acc) ->
    check_no_return([Object], State),
    [check_no_return([Expr], State) || {record_field, _, _Field, Expr} <- Fields],
    expr(Next, State, [Form | Acc]);

expr([{record_field, _, Object, _Name, Field} = Form | Next], State, Acc) ->
    check_no_return([Object, Field], State),
    expr(Next, State, [Form | Acc]);

expr([{op, _, _, Op} = Form | Next], State, Acc) ->
    check_no_return([Op], State),
    expr(Next, State, [Form | Acc]);

expr([{op, _, _, Op1, Op2} = Form | Next], State, Acc) ->
    check_no_return([Op1, Op2], State),
    expr(Next, State, [Form | Acc]);

expr([{tuple, _, Expressions} = Form | Next], State, Acc) ->
    check_no_return(Expressions, State),
    expr(Next, State, [Form | Acc]);

expr([{cons, _, Head, Tail} = Form | Next], State, Acc) ->
    check_no_return([Head, Tail], State),
    expr(Next, State, [Form | Acc]);

expr([Form | Next], State, Acc) ->
    case is_simple_form(Form) of
        true ->
            expr(Next, State, [Form | Acc]);
        _ ->
            msg_error(element(2, Form), State, {not_supported_expression, Form, form_size, size(Form)})
    end.


is_simple_form({atom, _, Atom}) when is_atom(Atom) -> true;
is_simple_form({var, _, Atom}) when is_atom(Atom)  -> true;
is_simple_form({integer, _, _})                    -> true;
is_simple_form({string, _, _})                     -> true;
is_simple_form({nil, _})                           -> true;
is_simple_form({'fun', _, {function, F, A}})
  when is_atom(F), is_integer(A)                   -> true;
is_simple_form({char, _, _})                       -> true;
is_simple_form({bin, _, _})                        -> true;
is_simple_form(_)                                  -> false.


check_no_return(Expressions, State) ->
    expr(Expressions, State#state{return=false,no_support=Expressions}, []).


control_clauses(State, Clauses) ->
    {NewClauses, _} = mapfoldl(fun clause/2, State, Clauses),
    {calc_state(State, NewClauses), NewClauses}.


calc_state(State, Clauses) ->
    Rs = lists:usort([R || {R, _} <- Clauses]),
    case lists:usort(Rs) of
        [true] ->
            State#state{return=true};
        [false] ->
            State#state{return=false};
        _ ->
            State#state{return=maybe}
    end.


apply_after_expressions([{'case', L, Expression, Clauses} | Other], AfterFun) ->
    [{'case', L, Expression, apply_in_clauses(Clauses, AfterFun)} | Other];
apply_after_expressions([{'if', L, Clauses} | Other], AfterFun) ->
    [{'if', L, apply_in_clauses(Clauses, AfterFun)} | Other];
apply_after_expressions([Last | Expressions], AfterFun) ->
    AfterFun(Last) ++ Expressions.


apply_in_clauses([{maybe, {clause, L, Params, Guards, Expressions}} | Next], AfterFun) ->
    NewExpressions = apply_after_expressions(Expressions, AfterFun),
    [{maybe, {clause, L, Params, Guards, NewExpressions}} | apply_in_clauses(Next, AfterFun)];
apply_in_clauses([{false, {clause, L, Params, Guards, [Last | Expressions]}} | Next], AfterFun) ->
    [{false, {clause, L, Params, Guards, AfterFun(Last) ++ Expressions}}
     | apply_in_clauses(Next, AfterFun)];
apply_in_clauses([Clause | Next], AfterFun) ->
    [Clause | apply_in_clauses(Next, AfterFun)];
apply_in_clauses([], _) ->
    [].


clean(State) ->
    State#state{return=false}.

combine(#state{return=R1, filename=Filename, only_remote=OnlyRemote}=State1,
        #state{return=R2, filename=Filename, only_remote=OnlyRemote}=State2) ->
    ?DEBUG("combine: ~p + ~p\n", [R1, R2]),
    if R1 =:= R2 ->
            State1;
       %% R1 =:= false, R2 =:= true ->
       %%      Statea;
       R1 =:= maybe, R2 =:= true ->
            State2;
       R1 =:= maybe, R2 =:= false ->
            State1
    end.


msg_error(L, State, Msg) ->
    throw({error, [{State#state.filename,
                    [{L, compile,
                      {parse_transform, ?MODULE,
                       Msg}}]}], []}).

not_supported(L, State) ->
    msg_error(L, State, {"return not supported", State#state.no_support}).


%%% From parse_trans by Ulf Wiger
%%% Slightly modified version of lists:mapfoldl/3
%%% Here, F/2 is able to insert forms before and after the form
%%% in question. The inserted forms are not transformed afterwards.
mapfoldl(F, Accu0, [Hd | Tail]) ->
    case F(Hd, Accu0) of
        {Before, Res1, After, Accu1} when is_list(Before), is_list(After) ->
            ok;
        {Res1, Accu1} ->
            Before = After = []
    end,
    {Res2, Accu2} = mapfoldl(F, Accu1, Tail),
    Res3 = After ++ Res2,
    Res4 = if is_list(Res1) -> Res1 ++ Res3;
              true          -> [Res1 | Res3]
           end,
    {Before ++ Res4, Accu2};
mapfoldl(F, Accu, []) when is_function(F, 2) ->
    {[], Accu}.


clean_clauses([]) ->
    [];
clean_clauses([{_, {clause, L, Params, Guards, Expressions}} | Next]) ->
    [{clause, L, clean_clauses(Params), Guards, lists:reverse(clean_clauses(Expressions))}
     | clean_clauses(Next)];
clean_clauses([T | Next]) when is_tuple(T) ->
    [list_to_tuple(clean_clauses(tuple_to_list(T))) | clean_clauses(Next)];
clean_clauses([L | Next]) when is_list(L) ->
    [clean_clauses(L) | clean_clauses(Next)];
clean_clauses([I | Next]) ->
    [I | clean_clauses(Next)].
