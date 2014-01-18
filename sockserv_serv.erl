-module(sockserv_serv).
-behaviour(gen_server).

-record(state, {socket,worker}).

-export([start_link/1,stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


% --------------------
% execute() - Spawns a worker to work on a given expression
% --------------------
execute(Ex,_State) -> %send(Socket, "trying", []),
											  PID = spawn_link(sat_worker,reciever,[Ex,self()]),
											  PID.

% --------------------
% str_erl(S) - Converts a string to an erlang atom
% --------------------
str_erl(S) ->
	try
		Str = S ++ ".",
		{ok, Tokens, _Line} = erl_scan:string(Str),
		{ok, AbsForm} = erl_parse:parse_exprs(Tokens),
		{value, Value, _Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
		Value
	catch
		_Class:_Err -> error
	end.

% --------------------
% do_rpc(_State, RawData) - Does RPC on a given RawData
% --------------------
do_rpc(_State, RawData) ->
    try
		NewData = re:replace(RawData, "\r\n$", "", [{return, list}]), 
		str_erl(NewData)
    catch
        _Class:_Err ->
				error
    end.

% --------------------
% start_link(Socket) - Starts the server
% --------------------
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

% --------------------
% init(Socket) - Initializes the server
% --------------------
init(Socket) ->
		gen_server:cast(self(), accept),
		{ok, #state{socket=Socket}}.

% --------------------
% handle_call(_E, _From, State) - Handles all calls
% --------------------
handle_call(_E, _From, State) ->
    {noreply, State}.

% --------------------
% handle_cast(accept, ..) - Accepts a connection and decides if busy or not
% --------------------
handle_cast(accept, S = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
	TF = sat_server:child_allowed(),
	case (TF) of
		true -> send(AcceptSocket, "hello", []),
				sat_server:create_listeners(),
				{noreply, S#state{socket=AcceptSocket}, 60000};
		false -> send(AcceptSocket, "busy", []),
				 sat_server:create_listeners(),
				 {stop, normal, S}
	end.

% --------------------
% handle_info(tcp) - Handles TCP and decides whether to abort, spawn a worker, or ignore
% --------------------
handle_info({tcp, _Socket, RawData}, State = #state{socket=ListenSocket,worker=OldWorker}) ->
	Data = do_rpc(State,RawData),
	case OldWorker of
		undefined -> Worker = execute(Data,State),
					 {noreply, #state{socket=ListenSocket,worker=Worker}, 4000};
		_W when Data =:= abort ->
			exit(OldWorker,worker_down),
			send(ListenSocket,"aborted",[]),
			{noreply, #state{socket=ListenSocket,worker=undefined}};
		_ -> send(ListenSocket,"ignored",[]),
			{noreply, State, 4000}
	end;

% --------------------
% handle_info(tcp_closed) - When TCP is closed, stops
% --------------------
handle_info({tcp_closed,_Socket}, State) ->
		{stop, normal, State};

% --------------------
% handle_info(timeout) - When timeout is sent, it says trying or stops
% --------------------
handle_info(timeout, S = #state{socket=Socket,worker=Worker}) ->
	case Worker of
		undefined -> {stop,normal, S};
		_ -> send(Socket, "trying", []),
			{noreply, S, 4000}
	end;

% --------------------
% handle_info({worker,ignored}) - When the worker ignores, send ignored
% --------------------
handle_info({worker,ignored},_S = #state{socket=Socket,worker=Worker}) ->
	exit(Worker, worker_ignored),
	send(Socket, "ignored",[]),
	{noreply, #state{socket=Socket,worker=undefined}, 60000};

% --------------------
% handle_info({worker,trying}) - When the worker is trying, continue
% --------------------
handle_info({worker,trying},S) ->
	{noreply, S, 0};

% --------------------
% handle_info({worker,result, ...) - When the worker returns a result, print the result and reset tiemout clock
% --------------------
handle_info({worker,result, Info},_S = #state{socket=Socket,worker=Worker}) ->
	exit(Worker, worker_down),
	send(Socket, "~p", [Info]),
	{noreply, #state{socket=Socket,worker=undefined}, 60000};

% --------------------
% handle_info(E, S) - Handle any unexpected info
% --------------------
handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.

% --------------------
% code_change(_OldVsn, State, _Extra) - OPC Necessity
% --------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% --------------------
% terminate(normal, _State) - OPC Necessity
% --------------------
terminate(normal, _State) ->
    ok;

% --------------------
% terminate(_Reason, _State) - OPC Necessity
% --------------------
terminate(_Reason, _State) ->
    io:format("terminate reason: ~p~n", [_Reason]).

% --------------------
% stop() - Used in case we need to stop
% --------------------
stop() ->
	gen_server:cast(?MODULE, stop).

% --------------------
% send(Socket, Str, Args) - Helper function, used to send information to sockets
% --------------------
send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, true}]),
	ok.
