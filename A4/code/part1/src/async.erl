-module(async).

-export([new/2, loop/2, wait/1, poll/1, send/2]).

        

% Worker processs / Moderator process 
    % spawn(fun() -> Monitor ! {result, Fun(Arg)} end),


new(Fun, Arg) -> 
    % The initial state should be nothing such that poll returns nothing, 
    % The list contains all the receivers missing answer

    Monitor = spawn(fun() -> loop(nothing, []) end), 
    spawn(fun() -> 
            try Fun(Arg) of 
                Res -> Monitor ! {result, {ok, Res}}
            catch
                _ : Ex -> Monitor ! {exception, {exception, Ex}}
            end
          end ),
    Monitor. 

        % Monitor ! {result, Fun(Arg)} end),

    % Sends a result to the monitor as soon it's done
    % We want to communicate with the monitor from the terminal

% Basic request reply
request_reply(Request, Aid) ->
    Aid ! {Request, self()},
    receive
        {Res} -> Res
    end.

poll(Aid) -> request_reply(poll, Aid).

wait(Aid) -> request_reply(wait, Aid).


% Sends the answer to everyone on the list
send(_, []) -> 1;
send(Res, Lst) ->
    lists:last(Lst) ! {Res},
    loop(Res, lists:droplast(Lst)).


loop(State, Lst) ->
    receive 
        {exception, Ex} -> 
            spawn(fun() -> send(Ex , Lst) end),
            loop(Ex, []);

        {result, Res} -> 
            spawn(fun() -> send(Res, Lst) end), % When ever we get a result we send it to everyone
            loop(Res, []); % List of receivers are empty

        {poll, From} -> 
            From ! {State}, % Send the state / result whatever it is
            loop(State, Lst);

        {wait, From} ->
            case State of 
                nothing -> % In case we have no answer we should do anything 
                    Tmp = Lst ++ [From], % We add our receiver to our list of missing receivers
                    loop(State, Tmp); % We redo the loop with the missing receivers
                _ -> From ! {State}, % If we have an answer we send that directly
                     loop(State, Lst) % We redo the loop if 
            end
    end.

% async:new(fun(X) -> X / 0 end, 4).
% async:new(fun(X) -> timer:sleep(X) end, 5000).

% Monitor waits from the result from worker.