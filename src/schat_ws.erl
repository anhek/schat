-module(schat_ws).
-behaviour(cowboy_websocket).

%%% API
-export([send_message/2]).

%%% Cowboy callback API
-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
    username :: undefined | binary()
}).

%%%=====================================================================================================================
%%% API
%%%=====================================================================================================================

-spec send_message(pid(), binary()) -> ok | error.
send_message(Pid, Message) when is_pid(Pid) ->
    Message2 = jiffy:encode(Message),
    Message3 = {send_message, Message2},
    case catch Pid ! Message3 of
        Message3 -> ok;
        {'EXIT', {badarg, _}} -> error
    end.

%%%=====================================================================================================================
%%% Cowboy callback API
%%%=====================================================================================================================

init(Req, _Opts) ->
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, #state{username = undefined} = State) ->
    try
        Username = case jiffy:decode(Msg, [return_maps]) of
            #{<<"username">> := Val1} -> Val1;
            _ -> throw({app_error, <<"Register first">>})
        end,
        case is_valid_username(Username) of
            true -> void;
            false -> throw({app_error, <<"Username format not allowed">>})
        end,
        Members = schat_user_mng:get_all(),
        case schat_user_mng:register(Username) of
            ok -> void;
            {error, username_already_registered} -> throw({app_error, <<"Username already used">>})
        end,
        Reply = jiffy:encode(#{
            <<"status">> => <<"ok">>,
            <<"members">> => Members
        }),
        {reply, {text, Reply}, State#state{username = Username}}
    catch
        throw : {app_error, ErrorMessage} ->
            ErrorReply = format_error(ErrorMessage),
            {reply, {text, ErrorReply}, State};
        throw : {error, {_, invalid_json}} ->
            ErrorReply = format_error(<<"Invalid json">>),
            {reply, {text, ErrorReply}, State};
        throw : {error, {_, truncated_json}} ->
            ErrorReply = format_error(<<"Invalid json">>),
            {reply, {text, ErrorReply}, State}
    end;
websocket_handle({text, Msg}, #state{username = Username} = State) ->
    try
        {ToUsername, Message} = case jiffy:decode(Msg, [return_maps]) of
            #{<<"to">> := Val1, <<"message">> := Val2} -> {Val1, Val2};
            _ -> throw({app_error, <<"Wrong message format">>})
        end,
        case schat_user_mng:send_message(Username, ToUsername, Message) of
            ok -> void;
            {error, username_not_found} -> throw({app_error, <<"User not found">>})
        end,
        Reply = jiffy:encode(#{<<"status">> => <<"ok">>}),
        {reply, {text, Reply}, State}
    catch
        throw : {app_error, ErrorMessage} ->
            ErrorReply = format_error(ErrorMessage),
            {reply, {text, ErrorReply}, State};
        throw : {error, {_, invalid_json}} ->
            ErrorReply = format_error(<<"Invalid json">>),
            {reply, {text, ErrorReply}, State};
        throw : {error, {_, truncated_json}} ->
            ErrorReply = format_error(<<"Invalid json">>),
            {reply, {text, ErrorReply}, State}
    end;
websocket_handle(Data, State) ->
    logger:warning("Unexpected data ~p", [Data]),
    {ok, State}.

websocket_info({send_message, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(Info, State) ->
    logger:warning("Unexpected message ~p", [Info]),
    {ok, State}.

terminate(_Reason, _Req, #state{username = Username} = _State) when is_binary(Username) ->
    ok = schat_user_mng:unregister(Username);
terminate(_Reason, _Req, _State) ->
    ok.

%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================

-spec format_error(binary()) -> binary().
format_error(ErrorMessage) ->
    jiffy:encode(#{
        <<"status">> => <<"error">>,
        <<"message">> => ErrorMessage
    }).

-spec is_valid_username(binary()) -> boolean().
is_valid_username(Username) ->
    UsernameLength = byte_size(Username),
    case re:run(Username, <<"^[A-Za-z_]*$">>) of
        {match, [{0, UsernameLength}]} -> true;
        nomatch -> false
    end.

%%%=====================================================================================================================
%%% Tests
%%%=====================================================================================================================

is_valid_username_test_() ->
    [
        ?_assert(is_valid_username(<<"test_user">>)),
        ?_assert(is_valid_username(<<"Test_user">>)),
        ?_assert(is_valid_username(<<"test_User">>)),
        ?_assert(is_valid_username(<<"_test_user">>)),
        ?_assertNot(is_valid_username(<<"test_user0">>)),
        ?_assertNot(is_valid_username(<<"test-user">>)),
        ?_assertNot(is_valid_username(<<"test user">>))
    ].
