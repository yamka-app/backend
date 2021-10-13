%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.

-module(user_e).
-author("Yamka").
-license("MPL-2.0").
-description("The user entity").

-include("entity.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([get/1, get/2, search/1, update/2, email_in_use/1, create/4, create/3, online/1]).
-export([broadcast_status/1, broadcast_status/2]).
-export([send_friend_rq/2, decline_friend_rq/2, accept_friend_rq/2, remove_friend/2, block/2, unblock/2]).
-export([get_friends/1, get_pending_in/1, get_pending_out/1, get_blocked/1, get_groups/1]).
-export([add_dm_channel/2, remove_dm_channel/1]).
-export([start_email_confirmation/2, finish_email_confirmation/2]).
-export([find/2, cache_name/2, delete_cached_name/1]).
-export([get_note/2, set_note/3]).

%% returns true if the user is currently connected
online(Id) -> sweet_owners:is_online(Id).

%% broadcasts the user's status after they have logged in
broadcast_status(Id) ->
    #{status:=Status} = user_e:get(Id, false),
    broadcast_status(Id, Status).
broadcast_status(Id, Status) ->
    sweet_awareness:notify({user, Id}, #entity{type=user, fields=#{id=>Id, status=>Status}}).

%% checks if the specified E-Mail address is in use
email_in_use(EMail) ->
    {ok, User} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT email FROM users_by_email WHERE email=?",
        values    = [{email, EMail}]
    }),
    cqerl:size(User) > 0.

%% creates the user
create(Name, EMail, Password, BotOwner) ->
    Id = utils:gen_snowflake(),
    % hash the password
    Salt = crypto:strong_rand_bytes(32),
    PasswordHash = utils:hash_password(Password, Salt),
    % execute the CQL query
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO users (id,name,tag,email,salt,password,status,status_text,"
                    "ava_file,badges,bot_owner,email_confirmed,public) "
                    "VALUES (?,?,?,?,?,?,?,?,?,?,?,false,false)",
        values = [
            {id, Id},
            {name, Name},
            {tag, rand:uniform(99999)},
            {email, EMail},
            {salt, Salt},
            {password, PasswordHash},
            {status, 1},
            {status_text, ""},
            % generate a random avatar
            {ava_file, file_storage:register_file(utils:gen_avatar(), "user_avatar.png")},
            {badges, if BotOwner > 0 -> [3]; true -> [] end},
            {bot_owner, BotOwner},
            {fav_color, 0}
        ]
    }),
    % confirm email
    start_email_confirmation(Id, EMail),
    Id.

create(Name, EMail, Password) -> create(Name, EMail, Password, 0).

%% gets a user by ID
get(Id) -> get(Id, true).
get(Id, _QueryRelations=false) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM users WHERE id=?",
        values    = [{id, Id}]
    }),
    maps:from_list(cqerl:head(Rows));
get(Id, _QueryRelations=true) ->
    Row = get(Id, false),
    Vals = maps:merge(#{
        friends     => get_friends(Id),
        blocked     => get_blocked(Id),
        pending_in  => get_pending_in(Id),
        pending_out => get_pending_out(Id),
        groups      => get_groups(Id),
        badges      => [],
        mfa_secret  => null
    }, maps:filter(fun(_, V) -> V /= null end, Row)),
    % convert the status into its atomic representation
    #{status := StatusNum} = Vals,
    maps:put(status, maps:get(StatusNum, ?USER_STATUS_MAP), Vals).

%% gets a note set by someone
get_note(Id, By) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT note FROM user_notes WHERE user=? AND subject=?",
        values    = [{subject, Id}, {user, By}]
    }),
    case cqerl:head(Rows) of
        empty_dataset -> nonote;
        [{note, Note}] -> Note
    end.

%% sets a note
set_note(Id, By, Note) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "UPDATE user_notes SET note=? WHERE user=? AND subject=?",
        values    = [{subject, Id}, {user, By}, {note, Note}]
    }), ok.

%% searches a user by name and tag
search(NameTag) ->
    [Name, TagStr] = string:tokens(NameTag, "#"),
    Tag = list_to_integer(TagStr),
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        % ALLOW FILTERING should be fine, we're guaranteed to have <=1k users with the same name
        statement = "SELECT id FROM user_ids_by_name WHERE name=? AND tag=? ALLOW FILTERING",
        values    = [{name, Name}, {tag, Tag}]
    }),
    case cqerl:head(Rows) of
        [{id, Id}] -> {ok, Id};
        [] -> {error, nouser}
    end.

%% updates a user record
update(Id, Fields) ->
    {Str, Bind} = entity:construct_kv_str(Fields),
    Statement = "UPDATE users SET " ++ Str ++ " WHERE id=?",
    % de-atomize the status field if present
    Vals = [{K, case K of status->maps:get(V, utils:swap_map(?USER_STATUS_MAP));_->V end} || {K,V} <- Bind],
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = Statement,
        values    = [{id, Id}|Vals]
    }),
    % update name publicity
    PublicityUpdated = maps:is_key(public, Fields),
    if
        PublicityUpdated ->
            #{name := ExistingName} = user_e:get(Id, false),
            Public = maps:get(public, Fields),
            if
                Public -> cache_name(Id, ExistingName);
                true -> ok
            end;
        true ->
            delete_cached_name(Id)
    end,
    % cache the name if it got updated
    NameUpdated = maps:is_key(name, Fields),
    if NameUpdated ->
        Name = maps:get(name, Fields),
        #{groups := Groups, public := IsPublic} = user_e:get(Id, false),
        [group_e:cache_user_name(Group, Id, Name) || Group <- Groups],
        if IsPublic ->
            cache_name(Id, Name);
           true -> ok
        end;
       true -> ok
    end.

%% starts email confirmation
start_email_confirmation(Id, Addr) ->
    % 8 char long code
    Code = base64:encode(crypto:strong_rand_bytes(6)),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO email_conf (code, user) VALUES (?, ?)",
        values = [{code, Code}, {user, Id}]
    }),
    email:send_confirmation(Addr, Code),
    none.

%% starts email confirmation
finish_email_confirmation(Id, Code) ->
    Values = [{user, Id}, {code, Code}],
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT * FROM email_conf WHERE user=? AND code=?",
        values = Values
    }),
    case cqerl:head(Rows) of
        empty_dataset -> badcode;
        _ ->
            cqerl:run_query(erlang:get(cassandra), #cql_query{
                statement = "DELETE FROM email_conf WHERE user=? AND code=?",
                values = Values
            }),
            update(Id, #{email_confirmed => true}),
            ok
    end.

%% sends a friend request
send_friend_rq(From, To) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO friend_requests(user, subject) VALUES(?, ?)",
        values    = [{user, From}, {subject, To}]
    }).
%% declines or removes a friend request
decline_friend_rq(From, To) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM friend_requests WHERE user=? AND subject=?",
        values    = [{user, From}, {subject, To}]
    }).
%% accepts a friend request
accept_friend_rq(From, To) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "BEGIN BATCH "
            "DELETE FROM friend_requests WHERE subject=? AND user=?; "
            "INSERT INTO friendships(u1, u2) VALUES(?, ?); "
            "APPLY BATCH",
        values = [{user, From}, {subject, To}, {u1, To}, {u2, From}]
    }).
%% removes friend
remove_friend(User1, User2) ->
    % we don't know the order here
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM friendships WHERE u1=? AND u2=?",
        values = [{u1, User1}, {u2, User2}]
    }),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM friendships WHERE u1=? AND u2=?",
        values = [{u1, User2}, {u2, User1}]
    }).
%% blocks a user
block(Target, As) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO blocked(subject, user) VALUES(?, ?)",
        values = [{user, As}, {subject, Target}]
    }).
%% unblocks a user
unblock(Target, As) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM blocked(subject, user) VALUES(?, ?)",
        values = [{user, As}, {subject, Target}]
    }).

get_friends(Id) ->
    {ok, From1} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT u2 FROM friendships_by_u1 WHERE u1=?",
        values = [{u1, Id}]
    }),
    {ok, From2} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT u1 FROM friendships_by_u2 WHERE u2=?",
        values = [{u2, Id}]
    }),
    [U || [{u2, U}] <- cqerl:all_rows(From1)] ++
    [U || [{u1, U}] <- cqerl:all_rows(From2)].
get_pending_in(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT user FROM friend_requests_by_subject WHERE subject=?",
        values = [{subject, Id}]
    }), [U || [{user, U}] <- cqerl:all_rows(Rows)].
get_pending_out(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT subject FROM friend_requests_by_sender WHERE user=?",
        values = [{user, Id}]
    }), [U || [{subject, U}] <- cqerl:all_rows(Rows)].
get_blocked(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT subject FROM blocked WHERE user=?",
        values = [{user, Id}]
    }), [U || [{subject, U}] <- cqerl:all_rows(Rows)].
get_groups(Id) ->
    {ok, Rows} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT group FROM user_groups WHERE user=?",
        values = [{user, Id}]
    }), [G || [{group, G}] <- cqerl:all_rows(Rows)].




%% registers a DM channel
add_dm_channel([_,_]=Peers, Channel) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "INSERT INTO dm_channels(users, channel) VALUES (?, ?)",
        values    = [{users, Peers}, {channel, Channel}]
    }).
%% underegisters a DM channel
remove_dm_channel([_,_]=Peers) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM dm_channels(users) VALUES(?)",
        values = [{users, Peers}]
    }).

find(Name, Max) when Max > 5 ->
    find(Name, 5);
find(Name, Max) ->
    {FirstThree, Rest} = utils:split_mask_username(Name),
    Pattern = Rest ++ "%",
    {ok, Result} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "SELECT id FROM user_name_search WHERE first_three=? AND rest LIKE ? LIMIT ?",
        values = [
            {first_three, FirstThree},
            {rest, Pattern},
            {'[limit]', Max}
        ]
    }),
    [Id || [{id, Id}] <- cqerl:all_rows(Result)].
    
cache_name(User, Name) ->
    {FirstThree, Rest} = utils:split_username(Name),
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "UPDATE user_names SET first_three=?, rest=? WHERE id=?",
        values = [
            {id, User},
            {first_three, FirstThree},
            {rest, Rest}
        ]
    }).

delete_cached_name(Id) ->
    {ok, _} = cqerl:run_query(erlang:get(cassandra), #cql_query{
        statement = "DELETE FROM user_names WHERE id=?",
        values = [{id, Id}]
    }).