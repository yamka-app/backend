# Erlang Yamka backend

![Top language](https://img.shields.io/github/languages/top/yamka-app/backend)
![License](https://img.shields.io/github/license/yamka-app/backend)
![Platforms](https://img.shields.io/badge/platform-linux%20%7C%20windows%20%7C%20macos%20%7C%20freebsd-blueviolet)
![Last commit](https://img.shields.io/github/last-commit/yamka-app/backend)
![Status](https://img.shields.io/uptimerobot/status/m788461709-79464d516b1ef3af81a20454)
![Uptime (7 days)](https://img.shields.io/uptimerobot/ratio/7/m788461709-79464d516b1ef3af81a20454)
![Maintainer](https://img.shields.io/badge/maintainer-portasynthinca3-ff69b4)

## Structure
This project consists of three main parts, all of which are open-source, distributed and horizontally scalable:
  - Custom code (business logic) in the Erlang programming language
  - Database storage and search managed by Elassandra (a merge of Apache Cassandra and Elasticsearch)
  - User-uploaded file storage managed by GlusterFS

## Running
The entire project is containerized. Execute three commands to run it on your own machine:
```
git clone https://github.com/yamka-app/backend.git
cd backend
docker-compose build
docker-compose up
```

Some Elassandra configuration is required:
  - Set up authentication: create a user named `yamkadb` with a custom password ([this](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/configuration/secureConfigNativeAuth.html) tutorial might be helpful)
  - Execute the commands in `db_structure.cql`

Three secrets are required:
  - `tls_fullchain` - full TLS certificate chain (`.pem`)
  - `tls_privkey` - TLS certificate private key (`.pem`)
  - `cassandra_password` - Elassandra password

:construction: Your server instance is going to redirect your voice clients to our server because the domain names are hard coded in (at the moment). We're working on that. _While_ we're working on that, feel free to change the return value of `server_name/0` in `src/tasty/tasty.erl`.

## Control
The `admin` module provides some functions you can call from the shell:
  - `admin:powerup/0` starts this instance, begins accepting Sweet connections and doing other stuff
  - `admin:powerup/1` starts the instance but listens to the port provided as the argument to this function instead of the default one (1746).
  - `admin:powerdown/0` stops this instance, stops accepting Sweet connections, etc.
  - `admin:seed_nodes/1` with a `list(atom())` as its argument connects to other nodes in the cluster
  - `stats:stats/0` prints some stats

## Configuration
`app.config` in this project's root:
  - `sweet_port`: Sweet (main protocol) port (TCP)
  - `sweet_client_timeout`: clients get disconnected if they do not send packets for that many milliseconds
  - `sweet_comp_threshold`: server-to-client compression threshold (bytes)
  - `sweet_protocol_between`: `{MinimumProtocolVersion, MaximumProtocolVerion}` (inclusive)
  - `sweet_file_chunk_size`: server-to-client `FileDataChunk` packet payload size
  - `tasty_port`: Tasty (real-time, e.g. voice protocol) port (UDP)
  - `tasty_client_timeout`: clients get disconnected if they do not send packets for that many milliseconds
  - `tasty_packet_rate_limit`: packet rate limit (Hz)
  - `tasty_packet_sz_limit`: packet size limit (bytes)
  - `tasty_speaking_ind_threshold`: the "speaking" flag gets reset after that many milliseconds pass without the client sending voice
  packets
  - `email_relay`: e-mail server to send emails from
  - `stat_interval`: stat logging period (ms)
  - `token_ttl`: default access token expiration time (s)
  - `cassandra`: `{CassandraInstanceHostname, CassandraInstancePort}`
  - `typing_reset_threshold`: the "typing" flag gets reset after that many milliseconds pass without the client sending typing notifications
  - `file_storage_path`: user uploaded file storage path (preferrably distributed)

## Development and testing
`sync` is declared as a rebar dependency for this project, so you should be able to just edit your files, save the changes and watch your code get reloaded in the shell after you have started the project with `rebar3 shell`. You can also run `rebar3 check` to run xref and dialyzer.

## Credits
  - Website and backend hosting kindly provided by [FossHost](https://fosshost.org)