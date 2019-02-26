-module(s3_test).
-include_lib("eunit/include/eunit.hrl").

-define(assertContain(Sub, List), ?assert(does_contain(Sub, List))).
-define(assertS3List(Sub, S3Response), ?assertContain(Sub, ext_list(S3Response))).

integration_test_() ->
    {foreach,
     fun setup/0, fun teardown/1,
     [
      ?_test(get_put()),
      ?_test(concurrency_limit()),
      ?_test(timeout_retry()),
      ?_test(slow_endpoint()),
      ?_test(fold()),
      ?_test(list_objects()),
      ?_test(signed_url()),
      ?_test(reload_config())
     ]}.

setup() ->
    application:set_env(s3erl, s3_config, default_config()),
    s3_app:start(),
    ok.

teardown(_) ->
    s3_app:stop().

get_put() ->
    delete_if_existing(bucket(), <<"foo">>),

    ?assertEqual({ok, not_found}, s3:get(bucket(), <<"foo">>)),
    ?assertEqual({ok, not_found}, s3:get(bucket(), "foo")),

    ?assertMatch({ok, _}, s3:put(bucket(), <<"foo">>, <<"bazbar">>, "text/plain")),
    ?assertEqual({ok, <<"bazbar">>}, s3:get(bucket(), <<"foo">>)),

    ?assertMatch({ok, _}, s3:put(bucket(), <<"foo-copy">>, <<>>, "text/plain",
                                 5000, [{"x-amz-copy-source", bucket() ++ "/foo"}])).

concurrency_limit() ->
    Parent = self(),
    MaxConcurrencyCB = fun (N) -> Parent ! {max_concurrency, N} end,

    s3_app:stop(),
    application:set_env(s3erl, s3_config, [{max_concurrency_callback, MaxConcurrencyCB},
                                           {max_concurrency, 3}] ++ default_config()),

    s3_app:start(),

    meck:new(s3_lib),
    GetF = fun (_, _, _, _) -> timer:sleep(50), {ok, <<"bazbar">>} end,
    meck:expect(s3_lib, get, GetF),

    P1 = spawn(fun() -> Parent ! {self(), s3:get(bucket(), <<"foo">>)} end),
    P2 = spawn(fun() -> Parent ! {self(), s3:get(bucket(), <<"foo">>)} end),
    P3 = spawn(fun() -> Parent ! {self(), s3:get(bucket(), <<"foo">>)} end),
    P4 = spawn(fun() -> Parent ! {self(), s3:get(bucket(), <<"foo">>)} end),

    receive M0 -> ?assertEqual({max_concurrency, 3}, M0) end,
    ?assertEqual([{error, max_concurrency},
                  {ok, <<"bazbar">>},
                  {ok, <<"bazbar">>},
                  {ok, <<"bazbar">>}],
                 lists:sort([receive {P, M} -> M  end || P <- [P1,P2,P3,P4]])),

    ?assertEqual({ok, <<"bazbar">>}, s3:get(bucket(), <<"foo">>)),
    ?assertEqual({ok, [{puts, 0}, {gets, 4}, {deletes, 0}, {num_workers, 0}]},
                 s3_server:get_stats()),

    ?assert(meck:validate(s3_lib)),
    meck:unload(s3_lib).

timeout_retry() ->
    Parent = self(),
    RetryCb = fun (Reason, Attempt) ->
                      Parent ! {Reason, Attempt}
              end,
    s3_app:stop(),
    application:set_env(s3erl, s3_config, [{timeout, 10},
                                           {retry_callback, RetryCb},
                                           {retry_delay, 10}] ++ default_config()),
    s3_app:start(),

    meck:new(httpc),
    TimeoutF = fun (_, _, _, _) ->
                       timer:sleep(50),
                       {error, timeout}
               end,
    meck:expect(httpc, request, TimeoutF),

    ?assertEqual({error, timeout}, s3:get(bucket(), <<"foo">>)),

    receive M1 -> ?assertEqual({timeout, 0}, M1) end,
    receive M2 -> ?assertEqual({timeout, 1}, M2) end,
    receive M3 -> ?assertEqual({timeout, 2}, M3) end,
    receive _  -> ?assert(false) after 1 -> ok end,

    ?assert(meck:validate(httpc)),
    meck:unload(httpc).

slow_endpoint() ->
    Port = webserver:start(gen_tcp, [fun very_slow_response/5]),

    s3_app:stop(),
    application:set_env(s3erl, s3_config, [{timeout, 10},
                                           {endpoint, "localhost:" ++ integer_to_list(Port)},
                                           {retry_delay, 10}] ++ default_config()),
    s3_app:start(),

    ?assertEqual({error, timeout}, s3:get(bucket(), <<"foo">>, 100)).

list_objects() ->
    {ok, _} = s3:put(bucket(), "1/1", "foo", "text/plain"),
    {ok, _} = s3:put(bucket(), "1/2", "foo", "text/plain"),
    {ok, _} = s3:put(bucket(), "1/3", "foo", "text/plain"),
    {ok, _} = s3:put(bucket(), "2/1", "foo", "text/plain"),


    ?assertS3List([<<"1/1">>, <<"1/2">>, <<"1/3">>],
                   s3:list(bucket(), "1/", 10, "")),

    ?assertS3List([<<"1/3">>],
                  s3:list(bucket(), "1/", 10, "1/2")),

    %% List all, includes keys from other tests.
    ?assertS3List([<<"1/1">>, <<"1/2">>, <<"1/3">>, <<"2/1">>,
                   <<"foo">>, <<"foo-copy">>],
                  s3:list(bucket(), "", 10, "")).

fold() ->
        {ok, _} = s3:put(bucket(), "1/1", "foo", "text/plain"),
        {ok, _} = s3:put(bucket(), "1/2", "foo", "text/plain"),
        {ok, _} = s3:put(bucket(), "1/3", "foo", "text/plain"),
        {ok, _} = s3:put(bucket(), "2/1", "foo", "text/plain"),
    %% Depends on earlier tests to setup data.
    ?assertEqual([<<"1/3">>, <<"1/2">>, <<"1/1">>],
                 s3:fold(bucket(), "1/", fun(Key, Acc) -> [Key|Acc] end, [])),

     %% List all, includes keys from other tests.
    ?assertContain([<<"foo-copy">>, <<"foo">>, <<"2/1">>, <<"1/3">>, <<"1/2">>,
                    <<"1/1">>],
                   s3:fold(bucket(), "", fun(Key, Acc) -> [Key|Acc] end, [])).

callback_test() ->
    Parent = self(),
    F = fun (Request, Response, ElapsedUs) ->
                Parent ! {Request, Response, ElapsedUs}
        end,
    application:set_env(s3erl, s3_config, [{post_request_callback, F} | default_config()]),
    s3_app:start(),

    {ok, _} = s3:put(bucket(), "foo", "bar", "text/plain"),
    {ok, _} = s3:get(bucket(), "foo"),

    receive M1 ->
            fun () ->
                    {Request, Response, _ElapsedUs} = M1,
                    ?assertEqual({put, bucket(), "foo", "bar", "text/plain", []},
                                 Request),
                    ?assertMatch({ok, _}, Response)
            end()
    end,

    receive M2 ->
            fun () ->
                    {Request, Response, _ElapsedUs} = M2,
                    ?assertEqual({get, bucket(), "foo", []}, Request),
                    ?assertEqual({ok, <<"bar">>}, Response)
            end()
    end,

    s3_app:stop().

signed_url() ->
    delete_if_existing(bucket(), <<"sig">>),
    ?assertEqual({ok, not_found}, s3:get(bucket(), <<"sig">>)),
    {ok, _} = s3:put(bucket(), <<"sig">>, <<"signed_test">>, "text/plain"),

    {MegaSeconds, Seconds, _} = os:timestamp(),
    Expires = MegaSeconds * 1000000 + Seconds,
    Url = s3:signed_url(bucket(), <<"sig">>, Expires + 3600),

    ?assertMatch(
       {ok, {{_, 200, _}, _, "signed_test"}},
       httpc:request(Url)
      ).

reload_config() ->
    OldConfig = s3_server:get_config(),
    s3_server:reload_config([{max_retries, 5} | default_config()]),
    ?assertNotEqual(OldConfig, s3_server:get_config()).


%% HELPERS
very_slow_response(Module, Socket, _, _, _) ->
    timer:sleep(100),
    Module:send(
      Socket,
      "HTTP/1.1 200 OK\r\n"
      "Content-type: text/plain\r\nContent-length: 14\r\n\r\n"
      "Great success!"
    ).

delete_if_existing(Bucket, Key) ->
    case s3:get(Bucket, Key) of
        {ok, not_found} ->
            ok;
        {ok, _Doc} ->
            {ok, _} = s3:delete(Bucket, Key)
    end.

default_config() ->
    [{endpoint, "localhost:4569"} | credentials()].

credentials() ->
    [
     {access_key, "access_key"},
     {secret_access_key, "secret_access_key"}
    ].

bucket() ->
    File = filename:join([code:priv_dir(s3erl), "bucket.term"]),
    {ok, Config} = file:consult(File),
    proplists:get_value(bucket, Config).

ext_list({ok, List}) when is_list(List) ->
    List.

does_contain(Sub, List) when is_list(Sub),
                             is_list(List) ->
    lists:all(fun(E) -> lists:member(E, List) end, Sub) orelse
        [{sublist, Sub},
         {list, List}].
