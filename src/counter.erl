%%%--------------------------------------------------------------------------
%%% File       : counter.erl, version 1.00
%%% Author     : Donald Steven <>, adapted from code
%%%              suggested by Alpar Juttner and Witold Baryluk of the Erlang
%%%              community
%%% Purpose    : To demonstrate multiple independent counters
%%% Created    : 24 Apr 2016
%%% Invocation : erlc counter.erl
%%%              erl -noshell -s counter main -s init stop
%%%--------------------------------------------------------------------------

-module(counter).
-export([main/0,inc/1,dec/1,current/1,start/1,reset/1,loop/1]).

%%%--------------------------------------------------------------------------
%%% Fun     : inc(X), dec(X), current(X)
%%% Purpose : Increment, decrement the counter X, and provide its current
%%%           state
%%% Created : 24 Apr 2016
%%%--------------------------------------------------------------------------

inc(X) ->
     list_to_atom("counter_" ++ [X]) ! inc_counter,
     ok.

dec(X) ->
     list_to_atom("counter_" ++ [X]) ! dec_counter,
     ok.

current(X) ->
     list_to_atom("counter_" ++ [X]) ! {get_counter, self()},
     receive
         { counter_value, Cnt } ->
             Cnt
     end.

%% Adapataion LSINF2345
reset(X) ->
      list_to_atom("counter_" ++ [X]) ! rst_counter,
      ok.
%%%--------------------------------------------------------------------------
%%% Fun     : start(X) and loop(Counter)
%%% Purpose : Start the counter X
%%% Created : 24 Apr 2016
%%%--------------------------------------------------------------------------

start(X) ->

     Pid = spawn(counter,loop,[0]),
     Name = list_to_atom("counter_" ++ [X]),
     register(Name, Pid),
     ok.

loop(Counter) ->
     receive
         inc_counter ->
             NewCounter = Counter+1;
         dec_counter ->
             NewCounter = Counter-1;
         %% Adapataion LSINF2345
         rst_counter ->
             NewCounter = 0;
         {get_counter, Pid} ->
             Pid ! { counter_value, Counter },
             NewCounter = Counter
     end,
     loop(NewCounter).

%%%--------------------------------------------------------------------------
%%% Fun     : main()
%%% Purpose : Demonstrate usage
%%% Created : 24 Apr 2016
%%%--------------------------------------------------------------------------

main() ->

     I = 1,          % IMPORTANT: these are counter numbers, not values
     J = 2,          % They will be transliterated into "counter_1" and
                     % "counter_2" internal to the various funs

     start(I),       % start counters I and J
     start(J),       % all counters start at 0

     inc(I), inc(I),                 % increment and decrement 'out of order'
     inc(J), inc(J), inc(J), inc(J), % to verify counter independence
     inc(I),                         % results should be counter I = 2 and
     dec(J),                         % counter J = 3
     dec(I),

     io:format("Current counter I: ~p~n", [current(I)]),
     io:format("Current counter J: ~p~n", [current(J)]).

