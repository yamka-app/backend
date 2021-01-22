-module(user).
-author("Order").
-license("MPL-2.0").
-description("The user entity").

-include("entity.hrl").
-include("../packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, update/2, email_in_use/1, create/4, create/3]).

%% checks if the specified E-Mail address is in use
email_in_use(EMail) ->
    {ok, User} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT email FROM users WHERE email=?",
        values    = [{email, EMail}]
   }),
    cqerl:size(User) > 0.

%% creates the user
create(Name, EMail, Password, BotOwner) ->
    % generate random data
    Id   = utils:gen_snowflake(),
    Tag  = rand:uniform(99999),
    Salt = crypto:strong_rand_bytes(32),
    % hash the password
    PasswordHash = utils:hash_password(Password, Salt),
    % execute the CQL query
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO users (id, name, tag, email, salt, password, status, status_text,"
                      "ava_file, badges, bot_owner) VALUES (?,?,?,?,?,?,?,?,?,?,?)",
        values    = [
            {id, Id},
            {name, Name},
            {tag, Tag},
            {email, EMail},
            {salt, Salt},
            {password, PasswordHash},
            {status, 1},
            {status_text, ""},
            % generate a random avatar
            {ava_file, file_storage:register_file(utils:gen_avatar(), "user_avatar.png")},
            {badges, if BotOwner > 0 -> [3]; true -> [] end},
            {bot_owner, BotOwner}
        ]
   }),
    Id.

create(Name, EMail, Password) -> create(Name, EMail, Password, 0).

%% gets a user by ID
get(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM users WHERE id=?",
        values    = [{id, Id}]
   }),
    1 = cqerl:size(Rows),
    Row = maps:from_list(cqerl:head(Rows)),
    % insert default values
    Vals = maps:merge(#{
        friends     => [],
        blocked     => [],
        pending_in  => [],
        pending_out => [],
        dm_channels => [],
        groups      => [],
        badges      => []
   }, maps:filter(fun(_, V) -> V /= null end, Row)),
    % convert the status into its atomic representation
    #{status := StatusNum} = Vals,
    maps:put(status, maps:get(StatusNum, ?USER_STATUS_MAP), Vals).

%% updates a user record
update(Id, Fields) ->
    {Str, Bind} = entity:construct_kv_str(Fields),
    Statement = "UPDATE users SET " ++ Str ++ " WHERE id=?",
    % de-atomize the status field if present
    Vals = [{K, case K of status->maps:get(V, utils:swap_map(?USER_STATUS_MAP));_->V end} || {K,V} <- Bind],
    logging:log("~p ~p ~p", [Id, Vals, Statement]),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = Statement,
        values    = [{id, Id}|Vals]
    }).