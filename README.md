# Erlang Yamka backend

<img src="promo/logo_color_on_transparency.png" alt="yamka logo" width="256">

![Top language](https://img.shields.io/github/languages/top/yamka-app/backend)
![OTP 24](https://img.shields.io/badge/OTP%20version-24-red)
![License](https://img.shields.io/github/license/yamka-app/backend)
![Last commit](https://img.shields.io/github/last-commit/yamka-app/backend)
![Status](https://img.shields.io/uptimerobot/status/m788461709-79464d516b1ef3af81a20454)
![Uptime (7 days)](https://img.shields.io/uptimerobot/ratio/7/m788461709-79464d516b1ef3af81a20454)
![Maintainer](https://img.shields.io/badge/maintainer-portasynthinca3-ff69b4)

## Structure
This project consists of three main parts, all of which are open-source, distributed and horizontally scalable:
  - Custom code in the Erlang programming language (Erlang/OTP version 24)
  - Database storage and search managed by Scylla, a faster drop-in alternative to Cassandra
  - User-uploaded file storage managed by GlusterFS

## Running
Yamka and Scylla are containerized, Gluster isn't. You also need to set up a mail server; how you do this is outside the scope of this document. Execute these commands to download everything:
```
apt install glusterfs-server # or another package manager
git clone https://github.com/yamka-app/backend.git
cd backend
systemctl start glusterd # or something else if you're not using systemd
# (configure Yamka and Gluster here)
docker-compose build
docker-compose up -d

# (configure Scylla here, then restart the Yamka container)
```

Required Scylla configuration:
  - Set up authentication: create a user named `yamkadb` with a custom password ([this](https://docs.datastax.com/en/cassandra-oss/3.0/cassandra/configuration/secureConfigNativeAuth.html) tutorial might be helpful)
  - Execute `db_structure.cql` (e.g. `cqlsh --file db_structure.cql`)

Required Docker secrets:
  - `tls_fullchain` - full TLS certificate chain (`.pem`). You have to obtain it yourself (e.g. from [Let's Encrypt](https://letsencrypt.org/)).
  - `tls_privkey` - TLS certificate private key (`.pem`)
  - `cassandra_password` - Scylla password for the `yamkadb` user
  - `smtp_pass` - SMTP password for the `noreply` user on your mail server

Tweak the paths to these secrets in `docker-compose.yml`.

## Configuration
`src/yamkabackend.app.src`, key `env`:
  - `sweet_port`: Sweet (main protocol) port (TCP). Remember to open this port in the firewall if using one
  - `sweet_client_timeout`: clients get disconnected if they do not send packets for that many milliseconds
  - `sweet_comp_threshold`: server-to-client compression threshold (bytes)
  - `sweet_protocol_between`: `{MinimumProtocolVersion, MaximumProtocolVerion}` (inclusive). Making these values go over the default bounds will most certainly make the server unexpectedly crash in weird places.
  - `sweet_file_chunk_size`: server-to-client `FileDataChunk` packet payload size
  - `tasty_port`: Tasty (real-time, e.g. voice protocol) port (UDP). Remember to open this port in the firewall if using one
  - `tasty_client_timeout`: clients get disconnected if they do not send packets for that many milliseconds
  - `tasty_packet_rate_limit`: packet rate limit (Hz)
  - `tasty_packet_sz_limit`: packet size limit (bytes)
  - `tasty_speaking_ind_threshold`: the "speaking" flag gets reset after that many milliseconds pass without the client sending voice
  packets
  - `email_relay`: server to send email from
  - `stat_interval`: stat logging period (ms)
  - `token_ttl`: default access token expiration time (s)
  - `cassandra`: `{CassandraInstanceHostname, CassandraInstancePort}`
  - `typing_reset_threshold`: the "typing" flag gets reset after that many milliseconds pass without the client sending typing notifications
  - `file_storage_path`: user uploaded file storage path (preferrably a Gluster instance)

## Development and testing
`sync` is declared as a rebar dependency for this project, so you should be able to just edit your files, save the changes and watch your code get reloaded in the shell after you have started the project with `rebar3 shell`. You can also run `rebar3 check` to run xref and dialyzer. Scylla and Gluster have to be up, and the Yamka container has to be down.

## Credits
  - Website and backend hosting kindly provided by [FossHost](https://fosshost.org)