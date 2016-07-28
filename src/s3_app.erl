%%% @doc
%%% Application module for S3 client
%%% @end
-module(s3_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

%% API
start() ->
    application:ensure_all_started(s3erl).

stop() ->
    application:stop(s3erl).

%%% Application callbacks


%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. Calls top level supervisor.
%% @end
start(_StartType, _StartArgs) ->
    case s3_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%% @end
stop(_State) ->
    ok.

%%% Internal functions
