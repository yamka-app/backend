-module(auth).
-author("Order").
-license("MPL-2.0").
-description("Handles access tokens").

-include("packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([create_token/2, get_token/1, token_get_worker/3]).

-spec create_token(Permissions::list(atom()), UserId::integer()) -> unicode:charlist().
create_token(Permissions, UserId) ->
    % generate the token and hash it
    TokenBytes = crypto:strong_rand_bytes(48), % 48 bytes fit nicely in 64 base64 chars
    TokenHash  = utils:hash_token(TokenBytes),
    TokenStr   = base64:encode(TokenBytes),
    % write it to the database
    { ok, _ } = cqerl:run_query(get(cassandra), #cql_query{
        statement = "INSERT INTO tokens (id, hash, permissions) VALUES (?,?,?)",
        values    = [
            { id, UserId },
            { hash, TokenHash },
            { permissions, [maps:get(C, ?REVERSE_TOKEN_PERMISSION_MAP) || C <- Permissions] }
        ]
    }),
    TokenStr.

token_get_worker(Token, Cassandra, Pid) ->
    % hash the token
    TokenBytes = base64:decode(Token),
    TokenHash  = utils:hash_token(TokenBytes),
    % perform the query
    { ok, Rows } = cqerl:run_query(Cassandra, #cql_query{
        statement = "SELECT id, permissions FROM tokens WHERE hash=?",
        values    = [
            { hash, TokenHash }
        ]
    }),
    1 = cqerl:size(Rows),
    [{ id, Id }, { permissions, Perms }] = cqerl:head(Rows),
    Pid ! { ok, { Id, Perms } }.

-spec get_token(Token::unicode:charlist()) -> { list(atom()), integer() } | error.
get_token(Token) ->
    { Pid, _ } = spawn_monitor(?MODULE, token_get_worker, [Token, get(cassandra), self()]),
    receive
        { 'DOWN', _, process, Pid, Reason } when Reason /= normal -> error;
        { ok, R } -> R
    end.