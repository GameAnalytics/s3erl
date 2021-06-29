%%
%% Blocking stateless library functions for working with Amazon S3.
%%
-module(s3_lib).

%% API
-export([get/4, put/6, delete/3, list/5]).
-export([signed_url/4, signed_url/5]).

-include_lib("xmerl/include/xmerl.hrl").
-include("../include/s3.hrl").

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
-define(CRYPTO_MAC, true).
-endif. %% OTP_RELEASE < 23 (crypto:mac/4 unavailable)

-if(?OTP_RELEASE >= 21).
-define(USE_URI_STRING, true).
-endif. %% OTP_RELEASE < 21 (uri_string module is unavailable)
-endif. %% OTP_RELEASE

%%
%% API
%%

-spec get(#config{}, bucket(), key(), [header()]) ->
                 {ok, body()} | {error, any()}.
get(Config, Bucket, Key, Headers) ->
    do_get(Config, Bucket, Key, Headers).

-spec put(#config{}, bucket(), key(), body(), contenttype(), [header()]) ->
    {ok, etag()} | {error, any()}.
put(Config, Bucket, Key, Value, ContentType, Headers) ->
    MD5 = md5_hash_string(Value),
    NewHeaders = [{"Content-Type", ContentType},
                  {"Content-MD5", MD5}
                  | Headers],
    do_put(Config, Bucket, Key, Value, NewHeaders).

delete(Config, Bucket, Key) ->
    do_delete(Config, Bucket, Key).

list(Config, Bucket, Prefix, MaxKeys, Marker) ->
    Key = ["?", "prefix=", Prefix, "&", "max-keys=", MaxKeys, "&marker=", Marker],
    case request(Config, get, Bucket, lists:flatten(Key), [], <<>>) of
        {ok, _Headers, Body} ->
            {XmlDoc, _Rest} = xmerl_scan:string(binary_to_list(Body)),
            Keys = lists:map(fun (#xmlText{value = K}) -> list_to_binary(K) end,
                             xmerl_xpath:string(
                               "/ListBucketResult/Contents/Key/text()", XmlDoc)),

            {ok, Keys};
        {ok, not_found} ->
            {ok, not_found};
        {error, _} = Error ->
            Error
    end.

signed_url(Config, Bucket, Key, Expires) ->
    signed_url(Config, Bucket, Key, get, Expires).

signed_url(Config, Bucket, Key, Method, Expires) ->
    Signature = sign(Config#config.secret_access_key,
                     stringToSign(Method, "", integer_to_list(Expires),
                                  Bucket, Key, "")),
    Url = build_full_url(Config#config.endpoint, Bucket, Key),
    Query = compose_query(Config#config.access_key, Expires, Signature),
    lists:flatten([Url, "?", Query]).


-ifdef(USE_URI_STRING).
compose_query(AccessKey, Expires, Signature) ->
    uri_string:compose_query([{"AWSAccessKeyId", AccessKey},
                              {"Expires", integer_to_list(Expires)},
                              {"Signature", binary_to_list(Signature)}]).
-else.
compose_query(AccessKey, Expires, Signature) ->
    lists:flatten(["AWSAccessKeyId=", AccessKey, "&",
                   "Expires=", integer_to_list(Expires), "&", "Signature=",
                   http_uri:encode(binary_to_list(Signature))]).
-endif.

%%
%% INTERNAL HELPERS
%%

do_put(Config, Bucket, Key, Value, Headers) ->
    case request(Config, put, Bucket, Key, Headers, Value) of
        {ok, RespHeaders, Body} ->
            case lists:keyfind("etag", 1, RespHeaders) of
                {"etag", Etag} ->
                    %% for objects
                    {ok, Etag};
                false when Key == "" andalso Value == "" ->
                    %% for bucket
                    ok;
                false when Value == "" orelse Value == <<>> ->
                    %% for bucket-to-bucket copy
                    {ok, parseCopyXml(Body)}
            end;
        {ok, not_found} -> %% eg. bucket doesn't exist.
            {ok, not_found};
        {error, _} = Error ->
            Error
    end.

do_get(Config, Bucket, Key, Headers) ->
    case request(Config, get, Bucket, Key, Headers, <<>>) of
        {ok, ResponseHeaders, Body} ->
            if Config#config.return_headers ->
                    {ok, ResponseHeaders, Body};
               true ->
                    {ok, Body}
            end;
        {ok, not_found} ->
            {ok, not_found};
        Error ->
            Error
    end.

do_delete(Config, Bucket, Key) ->
    request(Config, delete, Bucket, Key, [], <<>>).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

build_host(Bucket) ->
    [to_list(Bucket), ".s3.amazonaws.com"].

build_host(Endpoint, Bucket) ->
    [Endpoint, "/", to_list(Bucket)].

build_url(undefined, Bucket, Path) ->
    lists:flatten(["http://", build_host(Bucket), "/", Path]);
build_url(Endpoint, _Bucket, Path) ->
    lists:flatten(["http://", Endpoint, "/", to_list(Path)]).

build_full_url(undefined, Bucket, Path) ->
    lists:flatten(["http://", build_host(Bucket), "/", to_list(Path)]);
build_full_url(Endpoint, Bucket, Path) ->
    lists:flatten(["http://", build_host(Endpoint, Bucket), "/", to_list(Path)]).

request(Config, Method, Bucket, Path, Headers, Body) when is_list(Body) ->
    request(Config, Method, Bucket, Path, Headers, list_to_binary(Body));
request(Config, Method, Bucket, Path, Headers, Body) ->
    Date = httpd_util:rfc1123_date(),
    Url = build_url(Config#config.endpoint, Bucket, Path),
    ContentMD5 = case lists:keyfind("Content-MD5", 1, Headers) of
                     {_, MD5} ->
                         MD5;
                     false ->
                         ""
                 end,
    Signature = sign(Config#config.secret_access_key,
                     stringToSign(Method, ContentMD5,
                                  Date, Bucket, Path, Headers)),

    Auth = ["AWS ", Config#config.access_key, ":", Signature],
    FullHeaders = [{"Authorization", Auth},
                   {"Host", build_host(Bucket)},
                   {"Date", Date},
                   {"Connection", "keep-alive"}
                   | Headers],
    do_request(Url, Method, FullHeaders, Body, Config#config.timeout).

do_request(Url, Method, Headers, Body, Timeout) ->
    Headers2 = [{"Content-Length", integer_to_list(erlang:size(Body))} | Headers],
    Request = case Method of
        get -> {Url, Headers2};
        _ -> {Url, Headers2, "", Body}
    end,
    Options = [{body_format, binary}, {headers_as_is, true}],
    case httpc:request(Method, Request, [{timeout, Timeout}], Options) of
        {ok, {{_, 200, _}, ResponseHeaders, ResponseBody}} ->
            {ok, ResponseHeaders, ResponseBody};
        {ok, {{_, 204, "No Content" ++ _}, _, _}} ->
            {ok, not_found};
        {ok, {{_, 404, "Not Found" ++ _}, _, _}} ->
            {ok, not_found};
        {ok, {_, Code, _ResponseHeaders, <<>>}} ->
            {error, Code};
        {ok, {_Code, _ResponseHeaders, _ResponseBody} = ErrorResp} ->
            handle_error_response(ErrorResp);
        {error, Reason} ->
            {error, Reason}
    end.

handle_error_response({Code, _ResponseHeaders, ResponseBody}) ->
    {ErrText, Explanation} = parseErrorXml(ResponseBody),
    case {Code, ErrText} of
        {{_, 400, _}, "BadDigest"} ->
            {error, bad_digest};
        _ ->
            {error, {ErrText, Explanation}}
    end.

parseErrorXml(Xml) ->
    {XmlDoc, _Rest} = xmerl_scan:string(binary_to_list(Xml)),
    [#xmlText{value=ErrorCode}] = xmerl_xpath:string("/Error/Code/text()", XmlDoc),
    [#xmlText{value=ErrorMessage}] = xmerl_xpath:string("/Error/Message/text()",
                                                        XmlDoc),
    {ErrorCode, ErrorMessage}.


parseCopyXml(Xml) ->
    {XmlDoc, _Rest} = xmerl_scan:string(binary_to_list(Xml)),
    %% xmerl doesn't parse &quot; escape character very well
    case xmerl_xpath:string("/CopyObjectResult/ETag/text()", XmlDoc) of
        [#xmlText{value=Etag}, #xmlText{value="\""}] -> Etag ++ "\"";
        [#xmlText{value=Etag}] -> Etag
    end.


%%
%% Signing
%%

is_amz_header(<<"x-amz-", _/binary>>) -> true; %% this is not working.
is_amz_header("x-amz-"++ _)           -> true;
is_amz_header(_)                      -> false.

canonicalizedAmzHeaders("") -> "";
canonicalizedAmzHeaders(AllHeaders) ->
    AmzHeaders = [{string:to_lower(K),V} || {K,V} <- AllHeaders, is_amz_header(K)],
    Strings = lists:map(
                fun s3util:join/1,
                s3util:collapse(
                  lists:keysort(1, AmzHeaders) ) ),
    s3util:string_join(lists:map( fun (S) -> S ++ "\n" end, Strings), "").

canonicalizedResource("", "")       -> "/";
canonicalizedResource(Bucket, "")   -> ["/", Bucket, "/"];
canonicalizedResource(Bucket, Path) when is_list(Path) ->
    canonicalizedResource(Bucket, list_to_binary(Path));
canonicalizedResource(Bucket, Path) ->
    case binary:split(Path, <<"?">>) of
        [URL, _SubResource] ->
            %% TODO: Possible include the sub resource if it should be
            %% included
            ["/", Bucket, "/", URL];
        [URL] ->
            ["/", Bucket, "/", URL]
    end.

stringToSign(Verb, ContentMD5 = "", Date, Bucket = "", Path,
             OriginalHeaders = "") ->
    VerbString = string:to_upper(atom_to_list(Verb)),
    Parts = [VerbString, ContentMD5, "", Date,
             canonicalizedAmzHeaders(OriginalHeaders)],
    [s3util:string_join(Parts, "\n"), canonicalizedResource(Bucket, Path)];
stringToSign(Verb, ContentMD5, Date, Bucket, Path, OriginalHeaders) ->
    VerbString = string:to_upper(atom_to_list(Verb)),
    ContentType = proplists:get_value("Content-Type", OriginalHeaders, ""),
    Parts = [VerbString, ContentMD5, ContentType, Date,
             canonicalizedAmzHeaders(OriginalHeaders)],
    [s3util:string_join(Parts, "\n"), canonicalizedResource(Bucket, Path)].

-ifdef(CRYPTO_MAC).
sign(Key,Data) ->
    Mac = crypto:mac(hmac, sha, Key, Data),
    base64:encode(Mac).
-else.
sign(Key,Data) ->
    Mac = crypto:hmac(sha, Key, lists:flatten(Data)),
    base64:encode(Mac).
-endif.



to_list(B) when is_binary(B) ->
    binary_to_list(B);
to_list(L) ->
    L.

%%
%% MD5
%%

md5_hash_string(Data) ->
    base64:encode_to_string(erlang:md5(Data)).
