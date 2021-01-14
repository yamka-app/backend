-module(normal_client).
-author("Order").
-license("MPL-2.0").
-description("\"Normal protocol\" client process").

-define(TIMEOUT, 30*1000).
-define(EMAIL_REGEX, "(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|\\\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])").
-define(MIN_PROTO, 5).
-define(MAX_PROTO, 5).
-include("packets/packet.hrl").
-include_lib("cqerl/include/cqerl.hrl").

-export([client_init/2]).

%% handles client packets
handle_packet(Packet, ScopeRef) when Packet#packet.type == identification ->
    { _, awaiting_identification } = { { ScopeRef, status_packet:make_invalid_state(awaiting_identification, Packet) }, get(state) },
    Fields = Packet#packet.fields,
    Protocol = maps:get(protocol, Fields),
    if
        (Protocol > ?MAX_PROTO) or (Protocol < ?MIN_PROTO) ->
            status_packet:make(unsupported_proto, "Unsupported protocol version", Packet);
        true -> 
            put(protocol, Protocol),
            put(supports_comp, maps:get(supports_comp, Fields)),
            put(state, awaiting_login),
            none
    end;

handle_packet(Packet, ScopeRef) when Packet#packet.type == login ->
    % ensure proper connection state
    { _, awaiting_login } = { { ScopeRef, status_packet:make_invalid_state(awaiting_login, Packet) }, get(state) },
    Fields = Packet#packet.fields,
    % rate limiting
    { _, 1 } = { { ScopeRef, status_packet:make_rate_limiting(Packet) }, ratelimit:hit(login) },
    % get the user and ensure they're the only one with this email (could be none)
    { ok, User } = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT salt, password, mfa_secret, id FROM users WHERE email=?",
        values    = [{ email, maps:get(email, Fields) }]
    }),
    { _, 1 } = { { ScopeRef, status_packet:make(login_error, "Invalid E-Mail", Packet) }, cqerl:size(User) },
    Row = cqerl:head(User),
    % check the password the user sent us
    SentPass  = maps:get(password, Fields),
    Salt      = proplists:get_value(salt, Row),
    Password  = proplists:get_value(password, Row),
    MfaSecret = proplists:get_value(mfa_secret, Row),
    { _, Password } = { { ScopeRef, status_packet:make(login_error, "Invalid password", Packet) }, utils:hash_password(SentPass, Salt) },
    % set the state depending on whether the client has 2FA enabled or not
    case MfaSecret of
        null ->
            put(state, normal),
            #packet{ type = status, reply = Packet#packet.seq, fields = #{
                code => login_success,
                msg => "Logged in successfully" } };

        Token ->
            put(state, awaiting_totp),
            put(mfa_secret, Token),
            #packet{ type = status, reply = Packet#packet.seq, fields = #{
                code => mfa_required,
                msg => "A time-based one-time 6-digid password is required to login" } }
    end;

handle_packet(Packet, ScopeRef) when Packet#packet.type == signup ->
    % ensure proper connection state
    { _, awaiting_login } = { { ScopeRef, status_packet:make_invalid_state(awaiting_login, Packet) }, get(state) },
    Fields = Packet#packet.fields,
    % check if the E-Mail is valid
    EMail    = maps:get(email, Fields),
    EMailLen = length(EMail),
    { _, match, [{ 0, EMailLen }] } = { { ScopeRef, status_packet:make(signup_error, "Invalid E-Mail", Packet) }, re:run(EMail, ?EMAIL_REGEX) },
    % check password length (should be at least 6)
    PassLen = length(maps:get(password, Fields)),
    if PassLen < 6 -> status_packet:make(signup_error, "Use a longer password", Packet); true -> ok end,
    % check if someone is using this E-Mail address already
    { ok, User } = cqerl:run_query(get(cassandra), #cql_query{
        statement = "SELECT email FROM users WHERE email=?",
        values    = [{ email, EMail }]
    }),
    { _, 0 } = { { ScopeRef, status_packet:make(signup_error, "E-Mail is already in use", Packet) }, cqerl:size(User) },
    % generate random data
    Tag      = rand:uniform(99999),
    Salt     = crypto:strong_rand_bytes(32),
    Password = utils:hash_password(maps:get(password, Fields), Salt);

handle_packet(Packet, ScopeRef) ->
    #packet{ type = status, reply = Packet#packet.seq, fields = #{
        code => unknown_packet,
        msg => "Unknown packet code" } }.

%% the client loop
%% reads the client's packets and responds to them
client_loop() ->
    ScopeRef = make_ref(),
    % read a packet
    { ReaderPid, _ } = spawn_monitor(packet_iface, reader, [get(socket), get(protocol), self()]),
    DecodingStatus = receive
        { 'DOWN', _, process, ReaderPid, Reason } when Reason /= normal
            -> { error, Reason };
        { packet, P }              -> { ok, P };
        { decoding_error, S, Err } -> { error, decoding, S, Err }
        after ?TIMEOUT             -> exit(ReaderPid, normal), { error, timeout }
    end,

    % weed out errors
    State = get(state),
    ReplyWith = case DecodingStatus of
        { error, E } ->
            logging:log("connection to ~w closed (~p)", [get(client_ip), E]),
            ssl:close(get(socket)),
            stop;

        { error, decoding, Seq, Type, DErr } ->
            if
                (Type /= identification) and (State == awaiting_identification) ->
                    status_packet:make(invalid_connection_state, "Protocol version unknown or illegal");
                true ->
                    logging:warn("~w: decoding error of seq ~w (~p)", [get(client_ip), Seq, DErr]),
                    status_packet:make(packet_parsing_error, "Packet parsing failed. Please check structures with the docs", Seq)
            end;
        
        { ok, Packet } ->
            if
                (Packet#packet.type /= identification) and (State == awaiting_identification) ->
                    status_packet:make(invalid_connection_state, "Protocol version unknown or illegal");
                true ->
                    % thanks to José M at https://stackoverflow.com/a/65711977/8074626
                    % for this graceful match error handling technique
                    try
                        % terminate the connection if the client sends us too many packets
                        { _, 1 } = { { ScopeRef, close }, ratelimit:hit(close) },
                        % ignore the packet if the client sends us too many of them, but not as many to close the connection
                        { _, 1 } = { { ScopeRef, status_packet:make_rate_limiting(Packet) }, ratelimit:hit(packet) },
                        handle_packet(Packet, ScopeRef)
                    of
                        V -> V
                    catch
                        error:{ badmatch, { { ScopeRef, Response }, _ } } -> Response
                    end
            end
    end,

    % send the response packet
    DoNext = case ReplyWith of
        stop  -> stop;
        close -> ssl:close(get(socket)), stop;
        none  -> continue;
        ReplyPacket ->
            ReplySeq = put(seq, get(seq) + 1),
            { WriterPid, _ } = spawn_monitor(packet_iface, writer, [
                get(socket), ReplyPacket, ReplySeq,
                get(protocol), get(supports_comp), self()
            ]),
            receive
                { 'DOWN', _, process, WriterPid, _ } -> stop;
                { sent, ReplySeq }                   -> continue
            end
    end,

    % what to do next?
    case DoNext of
        stop     -> ok;
        continue -> client_loop()
    end.

%% client init function
client_init(TransportSocket, Cassandra) ->
    % finish the handshake
    { ok, Socket } = ssl:handshake(TransportSocket),
    { ok, { ClientIP, _ } } = ssl:peername(Socket),
    logging:log("~w connected to the normal server", [ClientIP]),

    % set initial state
    put(client_ip, ClientIP),
    put(socket, Socket),
    put(cassandra, Cassandra),
    put(protocol, 0), put(supports_comp, false),
    put(seq, 1),
    put(state, awaiting_identification),

    % make rate limiters
    ratelimit:make(packet,  { 50,  1000   }),
    ratelimit:make(close,   { 55,  1000   }),
    ratelimit:make(login,   { 5,   30000  }),
    ratelimit:make(entity,  { 150, 1000   }),
    ratelimit:make(message, { 20,  10     }),
    ratelimit:make(bot,     { 1,   120000 }),

    % run the client loop
    try client_loop()
    catch
        Ex:Type:Trace ->
            % what do we do in this situation?
            % no idea
            % at least let's log it so we can take a look later
            logging:err("Internal error: ~p", [{ Ex, Type, Trace }]),
            Packet = status_packet:make(internal_error, "Sorry, an internal server error has occured"),
            { WriterPid, _ } = spawn_monitor(packet_iface, writer, [
                get(socket), Packet, 1337,
                get(protocol), get(supports_comp), self()
            ]),
            receive
                { 'DOWN', _, process, WriterPid, _ } -> stop;
                { sent, 1337 }                       -> continue
            end,
            ssl:close(get(socket)),
            exit(crash)
    end.