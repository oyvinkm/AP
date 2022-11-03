-module(async).

% -export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1]).

% gen_server behaviour
% -export([init/1, callback_mode/0, computing/3, done/3]).
-compile(export_all).
% -export([init/1, handle_cast/2, handle_call/3]).

-behaviour(gen_statem).

callback_mode() -> state_functions.

new(Fun, Arg) -> 
	{ok, Aid} = gen_statem:start_link(?MODULE, {Fun, Arg}, []),
	Aid. % Aid == Pid

worker(Pid, Fun, Arg) ->
	try Fun(Arg) of
		Res -> gen_statem:cast(Pid, {done, Res})
	catch
		_ : Ex -> gen_statem:cast(Pid, {err, Ex})
	end.

init({Fun, Arg}) ->
	Pid = self(), %Aid == Pid
	spawn(fun() -> worker(Pid, Fun, Arg) end),
	{ok, computing, nothing}.

handle_res(A, Res) -> 
    if A =:= exception -> 
    	throw(Res);
    true -> 
    	Res
    end.

wait(Aid) -> {A, Res} = gen_statem:call(Aid, wait),
			 handle_res(A, Res).

wait_catch(Aid) -> try gen_statem:call(Aid, wait_catch) of
						Res -> Res
				   catch
						_ : Ex -> throw(Ex)
				   end.

poll(Aid) -> gen_statem:call(Aid, poll).

%Doesn't handle exceptions yet. maybe
wait_any(Aids) -> 
	Var = [{Aid, wait_catch(Aid)} || Aid <- Aids],
	Res = [{Aid, Res} || {Aid, {_, Res}} <- Var],
	Res.

computing({call, From}, poll, Res) ->
	{keep_state, Res, [{reply, From, {Res}}]};

computing({call, From}, wait, Res) ->
	{next_state, waiting, {From, Res}};

computing({call, From}, wait_catch, Res) ->
	{next_state, waiting_catch, {From, Res}};

computing(cast, {err, Ex}, _) ->
	{next_state, handle_err, Ex};

computing(cast, {done, Res}, _) ->
	{next_state, done, Res}.

waiting(cast, {done, Res}, {From, _}) -> %Data is our result, (nothing while computing) From is the senders process.
	gen_statem:reply(From, Res),
	{next_state, done, Res};

%waiting for wait
waiting(cast, {err, Ex}, {_, _}) -> %Data is our result, (nothing while computing) From is the senders process.
	throw(Ex).

%waiting for wait_catch
waiting_catch(cast, {done, Res}, {From, _}) -> %Data is our result, (nothing while computing) From is the senders process.
	gen_statem:reply(From, {ok, Res}),
	{next_state, done, Res};

waiting_catch(cast, {err, Ex}, {From, _}) -> %Data is our result, (nothing while computing) From is the senders process.
	gen_statem:reply(From, {exception, Ex}),
	{next_state, handle_err, Ex}.

done({call, From}, wait, Res) ->
	{keep_state, Res, [{reply, From, Res}]};

done({call, From}, wait_catch, Res) ->
	{keep_state, Res, [{reply, From, {ok,Res}}]};

done({call, From}, poll, Res) ->
	{keep_state, Res, [{reply, From, {ok, Res}}]}.

handle_err({call, From}, wait_catch, Ex) ->
	{keep_state, Ex, [{reply, From, {exception, Ex}}]};

handle_err({call, From}, wait, Ex) ->
	{keep_state, Ex, [{reply, From, {exception, Ex}}]};

handle_err({call, From}, poll, Ex) ->
	{keep_state, Ex, [{reply, From, {exception, Ex}}]}.



% done({call, From}, wait, Res) ->
% 	{}

% done(cast, {wait, Res}, {From, Dat}) ->
% 	gen_statem:reply(From, {ok, Res}).
	% {reply, Res, Res}.

