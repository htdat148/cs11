%%%-------------------------------------------------------------------
%%% @author dathuynh
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2024 20:55
%%%-------------------------------------------------------------------
-module(proc_sieve).
-author("dathuynh").

%% API
-export([generate/1]).
-export([sieve_loop/0]).

-spec generate(integer()) -> list().
generate(Max) ->
    List = lists:seq(2, Max),
    Pid = spawn(fun sieve_loop/0),
    [Pid !  {'number', Num} || Num <- List],
    Pid ! {'done', self()},
    loop([]).

loop(Acc) ->
    receive
        {'result', N} ->
            loop(Acc ++ [N]);
        {'last_chain', N} ->
            Acc ++ [N]
    end.

sieve_loop() ->
    receive
        {'number', N} ->
            sieve_loop({N, 'undefined'})
    end.

sieve_loop({N, NextPid} = Tuple) ->
    receive
        {'number', Number} when Number rem N == 0 ->
            sieve_loop(Tuple);
        {'number', _Number}=Msg when is_pid(NextPid)  ->
            NextPid ! Msg,
            sieve_loop(Tuple);
        {'number', _Number}=Msg when NextPid == 'undefined' ->
            NextPid_1 = spawn(fun sieve_loop/0),
            NextPid_1 ! Msg,
            sieve_loop({N, NextPid_1});
        {'done', Master} when is_pid(NextPid)->
            Master ! {'result', N},
            NextPid ! {'done', Master};
        {'done', Master} when NextPid == 'undefined' ->
            Master ! {'last_chain', N}
    end.