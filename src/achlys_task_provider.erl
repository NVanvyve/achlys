-module(achlys_task_provider).

-behaviour(gen_server).

%% API
-export([start_link/0]).
%% Adds the pmodnav_task to the working set
%% using the Achlys task model
-export([is_this_a_train/2]).
-export([count_number_of_trains/0]).
-export([read_lasp/0]).
-export([schedule_task/0]).
-export([compute_data/0]).

%% gen_server callbacks
-export([init/1 ,
         handle_call/3 ,
         handle_cast/2 ,
         handle_info/2 ,
         terminate/2 ,
         code_change/3]).

-define(SERVER , ?MODULE).
-define(UPDATE_TIME, 30000). %30s

-record(state , {}).

start_link() ->
    gen_server:start_link({local , ?SERVER} , ?MODULE , [] , []).

init([]) ->
    ok = schedule_task(),
    {ok , #state{}}.

handle_call(_Request , _From , State) ->
    {reply , ok , State}.

handle_cast({task, Task} , State) ->
    %% Task propagation to the cluster, including self
    achlys:bite(Task),
    {noreply , State};

handle_cast(_Request , State) ->
    {noreply , State}.

handle_info({task, Task} , State) ->
    %% Task propagation to the cluster, including self
    achlys:bite(Task),
    {noreply , State};
handle_info(_Info , State) ->
    {noreply , State}.

terminate(_Reason , _State) ->
    ok.

code_change(_OldVsn , State , _Extra) ->
    {ok , State}.

schedule_task() ->
  %% Declare an Achlys task that will be
  %% executed exactly once
  Task = achlys:declare(mycount
      , all
      , single 
      , fun() ->
          io:format("Hello World ~n", []),
          count_number_of_trains()

  end),
  %% Send the task to the current server module
  %% after a 0ms delay
  erlang:send_after(0, ?SERVER, {task, Task}),
  ok.

% Get the current timestamp
get_timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega*1000000 + Sec)*1000 + round(Micro/1000).

% distance sensor simulation
% Return a value between 0 and 100
get_pmod_value() ->
    rand:uniform(100).

% Return true if the value is less than 65, else false
value_to_boolean(Value) ->
    if
        Value < 65 ->
            true;
        true ->
            false
    end.

is_this_a_train(AccYes, AccNo) ->
    Sensor = value_to_boolean(get_pmod_value()),
    timer:sleep(1000),
    case Sensor of 
        true ->
            NewAccYes = AccYes + 1,
            if
                NewAccYes > 5 ->
                    %wait_end_of_train(1, 1),
                    true;
                true ->
                    is_this_a_train(NewAccYes, AccNo)
            end;
        false ->
            NewAccNo = AccNo + 1,
            Ratio = AccYes / NewAccNo,
            if 
                Ratio < 2 ->
                    is_this_a_train(0,0);
                true ->
                    is_this_a_train(AccYes, NewAccNo)
            end
    end.

wait_end_of_train(AccYes, AccNo) -> 
    Sensor = value_to_boolean(get_pmod_value()),
    timer:sleep(1000),
    Ratio = AccYes / AccNo,
    if 
        Ratio < 0.8 ->
            true;
        true ->
            case Sensor of 
                true ->
                    wait_end_of_train(AccYes + 1, AccNo);
                false ->
                    wait_end_of_train(AccYes, AccNo + 1)
            end
    end.

count_number_of_trains() ->
    count_number_of_trains(0, get_timestamp() + ?UPDATE_TIME).

count_number_of_trains(Acc, UpdateTime) ->
    is_this_a_train(0,0),
    CurrentTime = get_timestamp(),
    NewAcc = Acc + 1,
    if
        UpdateTime < CurrentTime ->
            io:format('Number of trains : ~p ~n', [Acc]),
            push_to_lasp(NewAcc),
            count_number_of_trains(0, get_timestamp() + ?UPDATE_TIME);
        true ->
            count_number_of_trains(NewAcc, UpdateTime)
    end.

read_lasp() ->
    Id = {<<"trains">>, state_gset},
    {ok, Result} = lasp:query(Id),
    sets:to_list(Result).

push_to_lasp(Count) ->
    Id = {<<"trains">>, state_gset},
    {ok, {_, _, _, _}} = lasp:declare(Id, state_gset),
    Name = node(),
    lasp:update(Id, {add, {Name, Count}}, self()).


compute_data(List, OurAcc, GlobalAcc) ->
   case List of
	   [] ->
		   io:format('List is empty !'),	
		   none;
	   [Head] ->
			{Node, Value} = Head, 
			if 
				Node == node() ->
					{OurAcc + Value, GlobalAcc + Value};
				true ->
					{OurAcc, GlobalAcc + Value}
			end;
	   [Head | Tail] ->
			{Node, Value} = Head, 
			if 
				Node == node() ->
					compute_data(Tail, OurAcc + Value, GlobalAcc + Value);
				true ->
					compute_data(Tail, OurAcc, GlobalAcc + Value)
			end
   end. 

compute_data() ->
	Id = {<<"trains">>, state_gset},
    {ok, Result} = lasp:query(Id),
    List = sets:to_list(Result),
	compute_data(List, 0, 0).
