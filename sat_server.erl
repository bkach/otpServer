%%%-------------------------------------------------------------------
%%% @author Boris Kachscovsky and Alan Ghobadi
%%% 	Built on a code base on: http://learnyousomeerlang.com 
%%% @doc RPC over TCP server. 
%%% @end
%%%-------------------------------------------------------------------

-module(sat_server).
-behaviour(supervisor).

-export([start/0, start_socket/0, create_listeners/0, child_allowed/0, num_children/0]).
-export([init/1]).

% --------------------
% start() - Starts the supervisor sat_server
% --------------------
start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% --------------------
% init([]) - Initializes one listener of sockserv_serv module 
% --------------------
init([]) ->
    {ok, Port} = {ok, 3547},
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {packet,line}]),
    spawn_link(fun empty_listeners/0),
    {ok, {{simple_one_for_one, 60, 3600},
         [{socket,
          {sockserv_serv, start_link, [ListenSocket]}, % pass the socket!
          temporary, 1000, worker, [sockserv_serv]}
         ]}}.

% --------------------
% start_socket() - Starts a child process
% --------------------
start_socket() ->
    supervisor:start_child(?MODULE, []).

% --------------------
% empty_listeners() - Creates empty listeners, can be used to create multiple listeners
% --------------------
empty_listeners() ->
	start_socket(),
    ok.

% --------------------
% create_listeners() - Alias for start_socket().
% --------------------
create_listeners() ->
	start_socket().

% --------------------
% num_children() - Returns the number of children
% --------------------
num_children() ->
	Children = supervisor:which_children(?MODULE),
	L = length(Children),
	L.

% --------------------
% child_allowed() - Returns true if a child is allowed, false otherwise
% --------------------
child_allowed() ->
	Children = num_children(),
	case Children of
		N when (N =< 8) -> true ;
		_ -> false
	end.
