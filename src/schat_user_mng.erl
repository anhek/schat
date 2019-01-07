-module(schat_user_mng).

%% API
-export([
    init/0,
    register/1,
    unregister/1,
    get_all/0,
    send_message/3
]).

-record(user, {
    name :: binary(),
    pid :: pid()
}).

%%%=====================================================================================================================
%%% API
%%%=====================================================================================================================

-spec init() -> ok.
init() ->
    Neighbours = get_neighbours(),
    {atomic, ok} = mnesia:create_table(user, [{ram_copies, Neighbours}, {attributes, record_info(fields, user)}]),
    ok.

-spec register(binary()) -> ok | {error, username_already_registered}.
register(Username) ->
    User = #user{
        name = Username,
        pid = self()
    },
    case get_user_pid(Username) of
        {ok, _Pid} ->
            {error, username_already_registered};
        {error, not_found} ->
            F = fun() ->
                ok = mnesia:write(user, User, write)
            end,
            {atomic, ok} = mnesia:transaction(F),
            ok
    end.

-spec unregister(binary()) -> ok.
unregister(UserName) ->
    F = fun() ->
        ok = mnesia:delete(user, UserName, write)
    end,
    {atomic, ok} = mnesia:transaction(F),
    ok.

-spec get_all() -> [binary()].
get_all() ->
    F = fun() -> mnesia:select(user, [{'_', [], ['$_']}]) end,
    Users = mnesia:activity(transaction, F),
    [Name || #user{name = Name} <- Users].

-spec send_message(binary(), binary(), binary()) -> ok | {error, username_not_found}.
send_message(FromUsername, ToUsername, Message) ->
    case get_user_pid(ToUsername) of
        {ok, Pid} ->
            Message2 = #{
                <<"from">> => FromUsername,
                <<"to">> => ToUsername,
                <<"message">> => Message,
                <<"created_at">> => os:system_time(second)
            },
            case schat_ws:send_message(Pid, Message2) of
                ok -> ok;
                error -> {error, username_not_found}
            end;
        {error, not_found} ->
            {error, username_not_found}
    end.


%%%=====================================================================================================================
%%% Internal functions
%%%=====================================================================================================================

-spec get_neighbours() -> [node()].
get_neighbours() ->
    InitialNodes = application:get_env(schat, nodes, []),
    lists:filter(fun(NodeName) ->
        case rpc:call(NodeName, application, which_applications, []) of
            {badrpc, _} -> false;
            RunningApps -> lists:keymember(schat, 1, RunningApps)
        end
    end, InitialNodes).


-spec get_user_pid(binary()) -> {ok, pid()} | {error, not_found}.
get_user_pid(Username) ->
    case mnesia:dirty_read(user, Username) of
        [#user{pid = Pid}] -> {ok, Pid};
        [] -> {error, not_found}
    end.
