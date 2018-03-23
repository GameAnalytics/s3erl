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

start(_StartType, _StartArgs) ->
    s3_sup:start_link().

stop(_State) ->
    ok.
