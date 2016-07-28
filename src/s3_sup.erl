%%% @doc
%%% Top level supervisor for the S3 client app.
%%% @end
-module(s3_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%% API functions

%% @doc
%% Starts the supervisor.
%% @end
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%% Supervisor callbacks

%% @private
%% @doc
%% Start an instance of s3_server.
%% @end
init([]) ->
    SupFlags = {one_for_one, 1000, 3600},

    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,

    S3Config = application:get_env(s3erl, s3_config, []),
    %% TODO: break down config into sensible bits.
    
    AChild = {s3_server, {s3_server, start_link, [S3Config]},
              Restart, Shutdown, Type, ['AModule']},

    {ok, {SupFlags, [AChild]}}.

%%% Internal functions
